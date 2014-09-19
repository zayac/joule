#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "TermConverter.h"

using namespace std;

using namespace llvm;
using namespace clang;

#include <iostream>
#include <numeric>
#include <algorithm>

string TermConverter::getTypeAsTerm(const QualType& ty) const {
    const QualType cty = ty.getCanonicalType();
    if (cty->isPointerType() && cty.isConstQualified())
        return "(ptr " + getTypeAsTerm(cty->getPointeeType()) + ")";
    else if (cty->isReferenceType() && cty.isConstQualified())
        return "(ref " + getTypeAsTerm(cty->getPointeeType()) +  ")";
    else if (cty->isBuiltinType()) { /* builtin types are always canonical */
        return "(" + string(cty.getCanonicalType().getAsString()) + ")";
    } else if (cty->isClassType()) {
        string s = cty.getCanonicalType().getAsString();
        return "\"" + s.substr(string("class ").size()) + "\"";
    } else if (cty->isStructureType()) {
        string s = cty.getCanonicalType().getAsString();
        return "\"" + s.substr(string("struct ").size()) + "\"";
    }
    assert(0);
}

bool TermConverter::isValidTerm(const QualType& ty) const {
    const QualType cty = ty.getCanonicalType();
    if ((cty->isPointerType() || cty->isReferenceType()) && cty.isConstQualified())
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

void TermConverter::parseRecord(const CXXRecordDecl *RD) {
    vector<FieldDecl> field_decls;
    if (RD->isClass() &&
            RD->isExternallyVisible() &&
           !RD->isAbstract()) {
        for (CXXRecordDecl::field_iterator fit = RD->field_begin(); fit != RD->field_end(); fit++) {
            FieldDecl field;
            if (fit->getAccess() == AS_public)
                field.access = Public_Access;
            else
                field.access = Private_Access;
            field.name = fit->getDeclName().getAsString();
            if (isValidTerm(fit->getType())) {
                field.type = getTypeAsTerm(fit->getType());
            } else {
                cerr << "Type of field '" << field.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
                exit(1);
            }
            field_decls.push_back(field);
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
                    params_terms.push_back(getTypeAsTerm(par.getOriginalType()));
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
        c.methods = method_decls;
        c.fields = field_decls;
        this->cl.push_back(c);
    }
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
        for (unsigned i = 0; i < c.fields.size(); i++) {
            FieldDecl field = c.fields[i];
            for (int i = 0; i < depth; i++)
                cout << "\t";
            cout << field.name << ": " << field.type;
            if (i != c.fields.size() - 1 || !c.methods.empty()) {
                cout << ", ";
            }
            cout << endl;
        }
        for (unsigned i = 0; i < c.methods.size(); i++) {
            MethodDecl method = c.methods[i];
            for (int i = 0; i < depth; i++)
                cout << "\t";
            cout << "\"(";
            if (method.is_const) {
                cout << "(const " << method.name << ")";
            } else {
                cout << method.name;
            }
            if (!method.params_terms.empty()) {
                for (unsigned j = 0; j < method.params_terms.size(); j++) {
                    replace(method.params_terms[j].begin(), method.params_terms[j].end(), '\"', '\'');
                    cout << " " << method.params_terms[j];
                }
            } else {
                cout << " void";
            }
            cout << ")\": " << method.return_term;
            if (i != c.methods.size() - 1) {
                cout << ", ";
            }
            cout << endl;
        }
        cout << "}";
    }
}

