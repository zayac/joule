#include "Interface.hpp"
#include <functional>
#include <sstream>

namespace interface {

static const std::string self = "\%self\%";

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

bool isStdContext(const DeclContext *context) {
    if (isa<NamespaceDecl>(*context)) {
        const NamespaceDecl *ns = static_cast<const NamespaceDecl*>(context);
        return ns->getNameAsString() == "std";
    }
    return false;
}

std::string typeToString(const QualType& ty, bool global_object_allowed) {
    QualType cty(ty.getCanonicalType());
    using namespace term;
    if (cty->isPointerType() && cty.isConstQualified()) {
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        if (s.substr(0, s.size() - 2) == deref.getAsString()) {
            if (deref.isConstQualified())
                return "const " + self + " * const";
            else
                return self + " * const";
        } else {
            return typeToString(cty->getPointeeType(), true) + " * const";
        }
    } else if (cty->isReferenceType()) {
        QualType deref = cty->getPointeeType();
        std::string s = ty.getAsString();
        if (s.substr(0, s.size() - 2) == deref.getAsString()) {
            if (deref.isConstQualified())
                return "const " + self + " &";
            else
                return "const " + self + " &";
        } else {
            return typeToString(cty->getPointeeType(), true) + " &";
        }
    } else if (cty->isBuiltinType()) { // builtin types are always canonical
        return cty.getCanonicalType().getAsString();
    /* Class declarations */
    } else if (cty->isClassType()) {
        const CXXRecordDecl *record = cty->getAsCXXRecordDecl();
        /*if ((isGlobalContext(record->getDeclContext()) || isStdContext(record->getDeclContext())) && !global_object_allowed) {
            std::cerr << "global (or std) object '" << record->getNameAsString()
                      << "' must be used only as a reference or a const pointer" << std::endl;
            exit(1);
        }*/
        global_object_allowed = false;

        if (!isGlobalContext(record->getDeclContext()) && !isStdContext(record->getDeclContext())) {
            std::cerr << "method parameters cannot contain interface objects or their pointers/references" << std::endl;
        }
        return "global::" + record->getNameAsString();
    } else {
        std::cerr << "unsupported type" << std::endl;
        exit(1);
    }

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
            tup.push_back(make_symbol(self));
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
            tup.push_back(make_symbol(self));
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
        /*if ((isGlobalContext(record->getDeclContext()) || isStdContext(record->getDeclContext())) && !global_object_allowed) {
            std::cerr << "global (or std) object '" << record->getNameAsString()
                      << "' must be used only as a reference or a const pointer" << std::endl;
            exit(1);
        }*/
        global_object_allowed = false;

        if (!isGlobalContext(record->getDeclContext()) && !isStdContext(record->getDeclContext())) {
            /* Instead of returning class representation as a term, we return
            * a term variable and generate auxiliary constraint */
            if (cached_classes.find(record->getNameAsString()) == cached_classes.end()) {
                cached_classes.insert(record->getNameAsString());
                constraints.insert(make_pair(make_var(record->getNameAsString()), classDeclToTerm(record, it)));
            }
            return make_var(record->getNameAsString());
        } else {
            return classDeclToTerm(record, it);
        }
    } else {
        std::cerr << "unsupported type" << std::endl;
        exit(1);
    }
}

inline std::string indent(unsigned depth) {
    return std::string(depth * 2, ' ');
}

std::string getStmtAsString(Stmt *stmt)
{
    // Might return until the beginning of the last token though
    SourceRange stmtRange = stmt->getSourceRange();
    std::string stmtString;

    int rangeSize = TheRewriter.getRangeSize(stmtRange);
    if (rangeSize == -1) {
        return "";
    }

    SourceLocation startLoc = stmtRange.getBegin();
    const char *strStart = TheSourceMgr->getCharacterData(startLoc);

    stmtString.assign(strStart, rangeSize);

    return stmtString;
}

std::unique_ptr<term::Term> classDeclToTerm(const CXXRecordDecl *RD, enum InterfaceType it) {
    if (isGlobalContext(RD->getDeclContext())) {
        return term::make_symbol("global::" + RD->getNameAsString());
    }

    if (isStdContext(RD->getDeclContext())) {
        return term::make_symbol("std::" + RD->getNameAsString());
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

        if (mit->isImplicit())
            continue;

        std::string method_name = mit->getDeclName().getAsString();
        if (method_name == RD->getNameAsString())
            method_name = self;
        else if (method_name[0] == '~')
            method_name = "~" + self;
        bool all_parameters_valid = true;
        method_name += "(";

        std::vector<std::string> param_names;

        for (FunctionDecl::param_const_iterator pit = mit->param_begin(); pit != mit->param_end(); pit++) {
            if (pit != mit->param_begin())
                method_name += ", "; 
            const ParmVarDecl par = **pit;

            param_names.emplace_back(par.getName());

            if (isValidType(par.getOriginalType())) {
                method_name += typeToString(par.getOriginalType(), false);
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

        //if (method_name == RD->getNameAsString() || method_name[0] == '~') 

        if (mit->doesThisDeclarationHaveABody()) {
            std::string body = getStmtAsString(mit->getBody());

            std::stringstream ss;
            ss << std::hash<std::string>()(body);

            /*std::string param_string = "";
            for (auto it = param_names.begin(); it != param_names.end(); ++it) {
                if (it != param_names.begin())
                    param_string += ",";
                param_string += *it;
            }*/

            std::string hash = "hash_" + ss.str();
            method_body[hash] = make_pair(param_names, body);

            //std::vector<std::unique_ptr<term::Term>> tup;
            //tup.emplace_back(term::make_symbol(typeToString(mit->getReturnType(), false)));

            std::unique_ptr<term::Term> code_term;
            if (mit->isVolatile()) {
                std::vector<std::unique_ptr<term::Term>> override_tuple;
                override_tuple.emplace_back(term::make_symbol("override"));
                override_tuple.emplace_back(term::make_symbol(hash));
                code_term = std::unique_ptr<term::Term>(new term::Tuple(override_tuple));
                //tup.emplace_back(std::unique_ptr<term::Term>(new term::Tuple(override_tuple)));
            } else
                code_term = term::make_symbol(hash);
                //tup.emplace_back(term::make_symbol(hash));

            auto prefix = std::mismatch(self.begin(), self.end(), method_name.begin());
            if (prefix.first == self.end() || method_name[0] == '~') {
                class_repr[method_name] = std::move(code_term);
            } else {
                std::vector<std::unique_ptr<term::Term>> tup;
                tup.emplace_back(term::make_symbol(typeToString(mit->getReturnType(), false)));
                tup.emplace_back(std::move(code_term));
                class_repr[method_name] = std::unique_ptr<term::Term>(new term::Tuple(tup));
            }

        } else {
            std::vector<std::unique_ptr<term::Term>> tup;
            tup.emplace_back(typeToTerm(mit->getReturnType(), it));
            tup.emplace_back(term::make_var("code_" + method_name));

            class_repr[method_name] = std::unique_ptr<term::Term>(new term::Tuple(tup));
        }

        //class_repr[method_name] = typeToTerm(mit->getReturnType(), it);
    }

    return std::unique_ptr<term::Term>(new term::Record(class_repr));
}

std::map<std::string, std::unique_ptr<term::Term>> getDeclFromFunctionDecl(const FunctionDecl* FD, enum InterfaceType it) {
    std::map<std::string, std::unique_ptr<term::Term>> struct_term;

    for (FunctionDecl::param_const_iterator pit = FD->param_begin(); pit != FD->param_end(); ++pit) {
        clang::Decl *param = *pit;
        const NamedDecl *namedDecl = dyn_cast<NamedDecl>(param);
        const ValueDecl *valueDecl = dyn_cast<ValueDecl>(param);

        if (namedDecl || valueDecl) {
            QualType declQT = valueDecl->getType().getCanonicalType();

            if (!interface::isValidType(declQT)) {
                std::cerr << "type " << declQT.getAsString() << " is not supported in the interface" << std::endl;
            }
            clang::ASTContext &context = param->getASTContext();
            struct_term[namedDecl->getNameAsString()] = typeToTerm(declQT, it);
        } else {
            std::cerr << "wrong declaration of function " << FD->getNameAsString() << std::endl;
        }
    }
    return struct_term;
}

}
