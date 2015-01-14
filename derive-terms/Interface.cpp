#include "Interface.h"

namespace interface {

bool isValidType(const QualType& ty) {
    const QualType cty = ty.getCanonicalType();
    if ((cty->isPointerType() && cty.isConstQualified()) || cty->isReferenceType())
        return isValidType(cty->getPointeeType());
    else if (cty->isBuiltinType()) {
        return true;
    } else if (cty->isClassType() || cty->isStructureType()) {
        const RecordDecl *RD = cty->getAs<RecordType>()->getDecl();
        if (RD->isExternallyVisible())
            return true;
    }
    return false;
}

bool isGlobalContext(const DeclContext *context) {
    if (isa<NamespaceDecl>(*context)) {
        const NamespaceDecl *ns = static_cast<const NamespaceDecl*>(context);
        return ns->getNameAsString() == "global";
    }
    return false;
}

std::unique_ptr<term::Term> typeToTerm(const QualType& ty, enum InterfaceType it) {
    QualType cty(ty.getCanonicalType());
    static bool global_object_allowed = false;
    using namespace term;
    if (cty->isPointerType() && cty.isConstQualified()) {
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        if (s.substr(0, s.size() - 2) == deref.getAsString()) {
            std::vector<std::unique_ptr<Term>> tup;
            tup.push_back(make_symbol("self"));
            tup.push_back(make_symbol("*const"));
            return std::unique_ptr<Term>(new Tuple(tup));
        } else {
            global_object_allowed = true;
            std::unique_ptr<Term> t = typeToTerm(cty->getPointeeType(), it);
            global_object_allowed = false;
            if (t) {
                std::vector<std::unique_ptr<Term>> tup;
                tup.push_back(std::move(t));
                tup.push_back(make_symbol("*const"));
                return std::unique_ptr<Term>(new Tuple(tup));
            } else {
                return nullptr;
            }
        }
    } else if (cty->isReferenceType()) {
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        if (s.substr(0, s.size() - 2) == deref.getAsString()) {
            std::vector<std::unique_ptr<Term>> tup;
            tup.push_back(make_symbol("self"));
            tup.push_back(make_symbol("&"));
            return std::unique_ptr<Term>(new Tuple(tup));
        } else {
            global_object_allowed = true;
            std::unique_ptr<Term> t = typeToTerm(cty->getPointeeType(), it);
            global_object_allowed = false;
            if (t) {
                std::vector<std::unique_ptr<Term>> tup;
                tup.push_back(std::move(t));
                tup.push_back(make_symbol("&"));
                return std::unique_ptr<Term>(new Tuple(tup));
            } else
                return nullptr;
        }
    } else if (cty->isBuiltinType()) { // builtin types are always canonical
        return std::unique_ptr<Term>(new Symbol(cty.getCanonicalType().getAsString()));
    /* Class declarations */
    } else if (cty->isClassType()) {
        const CXXRecordDecl *record = cty->getAsCXXRecordDecl();
        if (isGlobalContext(record->getDeclContext()) && !global_object_allowed) {
            std::cerr << "global object '" << record->getNameAsString()
                      << "' must be used only as a reference or a const pointer" << std::endl;
            exit(1);
        }
        global_object_allowed = false;
        /* Instead of returning class representation as a term, we return
         * a term variable and generate auxiliary constraint */
        constraints.insert(make_pair(std::unique_ptr<term::Term>(new Var(record->getNameAsString())), classDeclToTerm(record, it)));
        return std::unique_ptr<term::Term>(new Var(record->getNameAsString()));
    } else {
        std::cerr << "unsupported type" << std::endl;
        exit(1);
    }
}

inline std::string indent(unsigned depth) {
    return std::string(depth * 2, ' ');
}


std::unique_ptr<term::Term> classDeclToTerm(const CXXRecordDecl *RD, enum InterfaceType it) {
    if (isGlobalContext(RD->getDeclContext())) {
        return term::make_symbol("global::" + RD->getNameAsString());
    }

    std::map<std::string, std::unique_ptr<term::Term>> class_repr;
    // fields
    for (CXXRecordDecl::field_iterator fit = RD->field_begin(); fit != RD->field_end(); fit++) {
        if (!fit->getType().getCanonicalType().isVolatileQualified() || it == TOutputInterface) {
            /*std::unique_ptr<term::Term> field_decl;
            if (fit->getAccess() == AS_public)
                field_decl.push_back(term::make_symbol("public"));
            else
                field_decl.push_back(term::make_symbol("private"));*/

            QualType q = fit->getType().getCanonicalType();
            if (q.isVolatileQualified()) {
                q.removeLocalVolatile();
            }
            if (isValidType(q)) {
                class_repr[fit->getDeclName().getAsString()] = typeToTerm(q, it);
            } else {
                std::cerr << "Type of field '"
                          << fit->getDeclName().getAsString()
                          << "' that is a member of class '"
                          << RD->getNameAsString()
                          << "' does not have a valid term representation"
                          << std::endl;
                exit(1);
            }
        }
    }

    // classes
    for (CXXRecordDecl::decl_iterator dit = RD->decls_begin(); dit != RD->decls_end(); dit++) {
        if (isa<CXXRecordDecl>(*dit)) {
            const CXXRecordDecl *record = dyn_cast<CXXRecordDecl>(*dit);
            if (record->isExternallyVisible() && !RD->isAbstract() && !record->isInjectedClassName()) {
                class_repr[record->getNameAsString()] = classDeclToTerm(record, it);
            }
        }
    }
    
    //methods
    for(CXXRecordDecl::method_iterator mit = RD->method_begin(); mit != RD->method_end(); mit++) {
        /*std::vector<std::unique_ptr<term::Term>> method_decl;
        if (mit->getAccess() == AS_public)
            method_decl.push_back(term::make_symbol("public"));
        else
            method_decl.push_back(term::make_symbol("private"));*/
        if (mit->isImplicit())
            continue;

        std::string method_name = mit->getDeclName().getAsString();
        if (method_name == RD->getNameAsString())
            method_name = "constructor";
        else if (method_name[0] == '~')
            method_name = "destructor";
        bool all_parameters_valid = true;
        method_name += "(";

        for (FunctionDecl::param_const_iterator pit = mit->param_begin(); pit != mit->param_end(); pit++) {
            if (pit != mit->param_begin())
                method_name += ", "; 
            const ParmVarDecl par = **pit;
            if (isValidType(par.getOriginalType())) {
                method_name += term::toString(typeToTerm(par.getOriginalType(), it));
            } else {
                all_parameters_valid = false;
                break;
            }
        }
        method_name += ")";
        if (!all_parameters_valid) {
            std::cerr << "Some parameters of method '" << method_name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << std::endl;
            exit(1);
        }
        if (mit->isConst())
            method_name += " const";
        if (!isValidType(mit->getReturnType())) {
            std::cerr << "Return type of method '" << method_name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << std::endl;
            exit(1);
        }

        //method_decl.push_back(typeToTerm(mit->getReturnType(), it));
        class_repr[method_name] = typeToTerm(mit->getReturnType(), it);
    }

    return std::unique_ptr<term::Term>(new term::Record(class_repr));
}

}
