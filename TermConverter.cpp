#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "TermConverter.h"

using namespace std;

using namespace llvm;
using namespace clang;

#include <iostream>
#include <numeric>
#include <algorithm>

string TermConverter::getTypeAsTerm(const QualType& ty, bool quotation_marks = true) const {
    string result = "";
    QualType cty(ty.getCanonicalType());
    /* remove volatile qualifier */
    if (cty.isLocalVolatileQualified())
        cty.removeLocalVolatile();
    if (cty->isPointerType() && cty.isConstQualified()) {
        result += getTypeAsTerm(cty->getPointeeType(), false) + "* const";
    } else if (cty->isReferenceType())
        result += getTypeAsTerm(cty->getPointeeType(), false) +  "&";
    else if (cty->isBuiltinType()) { /* builtin types are always canonical */
        result += string(cty.getCanonicalType().getAsString());
    } else if (cty->isClassType()) {
        string s = cty.getCanonicalType().getAsString();
        result += s.substr(string("class ").size());
    } else if (cty->isStructureType()) {
        string s = cty.getCanonicalType().getAsString();
        result += s.substr(string("struct ").size());
    }
    /* Invalid term. Need to call `isValidTerm' first. */
    if (result.empty())
        assert(0);
    if (quotation_marks)
        result = "\"" + result + "\"";
    return result;
}

bool TermConverter::isValidTerm(const QualType& ty) const {
    const QualType cty = ty.getCanonicalType();
    if ((cty->isPointerType() && cty.isConstQualified()) || cty->isReferenceType())
        return isValidTerm(cty->getPointeeType());
    else if (cty->isBuiltinType()) {
        return true;
    } else if (cty->isClassType() || cty->isStructureType()) {
        const RecordDecl *RD = cty->getAs<RecordType>()->getDecl();
        if (RD->isExternallyVisible())
            return true;
    }
    return false;
}

TermConverter::ClassDecl TermConverter::parseRecord(const CXXRecordDecl *RD) {
    vector<FieldDecl> field_decls;
    for (CXXRecordDecl::field_iterator fit = RD->field_begin(); fit != RD->field_end(); fit++) {
        FieldDecl field;
        if (fit->getAccess() == AS_public)
            field.access = Public_Access;
        else
            field.access = Private_Access;
        if (fit->getType().getCanonicalType().isVolatileQualified())
            field.is_volatile = true;
        field.name = fit->getDeclName().getAsString();
        if (isValidTerm(fit->getType())) {
            field.type = getTypeAsTerm(fit->getType());
        } else {
            cerr << "Type of field '" << field.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
            exit(1);
        }
        field_decls.push_back(field);
    }
    vector<ClassDecl> class_decls;
    for (CXXRecordDecl::decl_iterator dit = RD->decls_begin(); dit != RD->decls_end(); dit++) {
        if (isa<CXXRecordDecl>(*dit)) {
            const CXXRecordDecl *record = dyn_cast<CXXRecordDecl>(*dit);
            if (record->isExternallyVisible() && !RD->isAbstract() && !record->isInjectedClassName()) {
                class_decls.push_back(parseRecord(record));
            }
        }
    }
    vector<MethodDecl> method_decls;
    for(CXXRecordDecl::method_iterator mit = RD->method_begin(); mit != RD->method_end(); mit++) {
        MethodDecl method;
        if (mit->getAccess() == AS_public)
            method.access = Public_Access;
        else
            method.access = Private_Access;
        method.name = mit->getDeclName().getAsString();
        // we don't want to add destructor to a term description
        if (method.name == "~" + RD->getNameAsString())
            continue;
        method.is_const = mit->isConst();
        if (isValidTerm(mit->getReturnType())) {
            method.return_term = getTypeAsTerm(mit->getReturnType());
        } else {
            cerr << "Return type of method '" << method.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
            exit(1);
        }
        bool all_parameters_valid = true;
        vector<string> params_terms;
        for (FunctionDecl::param_const_iterator pit = mit->param_begin(); pit != mit->param_end(); pit++) {
            const ParmVarDecl par = **pit;
            if (isValidTerm(par.getOriginalType())) {
                params_terms.push_back(getTypeAsTerm(par.getOriginalType(), false));
            } else {
                all_parameters_valid = false;
                break;
            }
        }
        if (all_parameters_valid) {
            method.params_terms = params_terms;
        } else {
            cerr << "Some parameters of method '" << method.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
            exit(1);
        }
        method_decls.push_back(method);
    }
    ClassDecl c;
    c.name = RD->getNameAsString();
    c.classes = class_decls;
    c.methods = method_decls;
    c.fields = field_decls;
    return c;
}

void TermConverter::addClass(ClassDecl new_class) {
    cl.push_back(new_class);
}

void TermConverter::printClassTerms() {
    for (vector<ClassDecl>::const_iterator it = this->cl.begin(); it != this->cl.end(); it++) {
        cout << "$" << it->name << " <= ";
        printClassTerm(*it, 1);
        cout << ";" << endl;
    }
}

void TermConverter::printClassTerm(const ClassDecl& c, int depth) {
    if (c.methods.empty() && c.fields.empty()) {
        cout << "nil";
    } else {
        cout << "{" << endl;
        for (unsigned i = 0; i < c.classes.size(); i++) {
            ClassDecl cl = c.classes[i];
            for (int i = 0; i < depth; i++)
                cout << "\t";
            cout << cl.name << ": ";
            printClassTerm(cl, depth + 1);
            cout << ", " << endl;
        }
        for (unsigned i = 0; i < c.fields.size(); i++) {
            FieldDecl field = c.fields[i];
            for (int i = 0; i < depth; i++)
                cout << "\t";
            cout << field.name << ": (";
            if (field.access == Public_Access)
                cout << "public ";
            else
                cout << "private ";
            if (field.is_volatile)
                cout << "volatile ";
            cout << field.type << ")";
            if (i != c.fields.size() - 1 || !c.methods.empty()) {
                cout << ", ";
            }
            cout << endl;
        }
        for (unsigned i = 0; i < c.methods.size(); i++) {
            MethodDecl method = c.methods[i];
            for (int i = 0; i < depth; i++)
                cout << "\t";
            cout << "\"" << method.name << "(";
            for (unsigned j = 0; j < method.params_terms.size(); j++) {
                cout << method.params_terms[j];
                if (j < method.params_terms.size() - 1)
                    cout << ", ";
            }
            cout << ")";
            if (method.is_const) {
                cout << " const";
            }
            cout << "\": (" << ((method.access == Public_Access) ? "public " : "private ") << method.return_term << ")";
            if (i != c.methods.size() - 1) {
                cout << ", ";
            }
            cout << endl;
        }
        for (int i = 0; i < depth - 1; i++)
            cout << "\t";
        cout << "}";
    }
}

