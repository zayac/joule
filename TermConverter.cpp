#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "TermConverter.h"

using namespace std;

using namespace llvm;
using namespace clang;

#include <iostream>
#include <numeric>

string TermConverter::getTypeAsTerm(const QualType& ty) const {
    const QualType cty = ty.getCanonicalType();
    if (cty->isPointerType() && cty.isConstQualified())
        return "(ptr " + getTypeAsTerm(cty->getPointeeType()) + ")";
    else if (cty->isReferenceType() && cty.isConstQualified())
        return "(ref " + getTypeAsTerm(cty->getPointeeType()) +  ")";
    else if (cty->isBuiltinType()) { /* builtin types are always canonical */
        return "(" + string(cty.getCanonicalType().getAsString()) + ")";
    } else if (cty->isStructureType()) {
        const RecordDecl *RD = cty->getAsStructureType()->getDecl();
        return RD->getName();
    }
}

bool TermConverter::isValidTerm(const QualType& ty) const {
    const QualType cty = ty.getCanonicalType();
    if ((cty->isPointerType() || cty->isReferenceType()) && cty.isConstQualified())
        return isValidTerm(cty->getPointeeType());
    else if (cty->isBuiltinType()) {
        return true;
    } else if (cty->isStructureType()) {
        const RecordDecl *RD = cty->getAsStructureType()->getDecl();
        if (RD->isExternallyVisible())
            return true;
    }
    return false;
}

void TermConverter::parseRecord(const CXXRecordDecl *RD) {
    vector<FieldDecl> field_decls;
    if (RD->isStruct() &&
            RD->isExternallyVisible() &&
           !RD->isAbstract()) {
        for (CXXRecordDecl::field_iterator fit = RD->field_begin(); fit != RD->field_end(); fit++) {
            if (fit->getAccess() == AS_public) {
                FieldDecl field;
                field.name = fit->getDeclName().getAsString();
                if (isValidTerm(fit->getType())) {
                    field.type = getTypeAsTerm(fit->getType());
                } else {
                    cerr << "Type of field '" << field.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
                    continue;
                }
                field_decls.push_back(field);
            }
        }
        vector<MethodDecl> method_decls;
        for(CXXRecordDecl::method_iterator mit = RD->method_begin(); mit != RD->method_end(); mit++) {
            if (mit->getAccess() == AS_public) {
                MethodDecl method;
                method.name = mit->getDeclName().getAsString();
                method.is_const = mit->isConst();
                if (isValidTerm(mit->getReturnType())) {
                    method.return_term = getTypeAsTerm(mit->getReturnType());
                } else {
                    cerr << "Return type of method '" << method.name << "' that is a member of class '" << RD->getNameAsString() << "' does not have a valid term representation" << endl;
                    break;
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
                    break;
                }
                method_decls.push_back(method);
            }
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
        if (it->methods.empty() && it->fields.empty()) {
            cout << "nil;" << endl;
        } else {
            cout << "{" << endl;
            for (unsigned i = 0; i < it->fields.size(); i++) {
                FieldDecl field = it->fields[i];
                cout << "\t" << field.name << ": " << field.type;
                if (i != it->fields.size() - 1 || !it->methods.empty()) {
                    cout << ", ";
                }
                cout << endl;
            }
            for (unsigned i = 0; i < it->methods.size(); i++) {
                MethodDecl method = it->methods[i];
                cout << "\t'(";
                if (method.is_const) {
                    cout << "(const " << method.name << ")";
                } else {
                    cout << method.name;
                }
                if (!method.params_terms.empty()) {
                    for (unsigned j = 0; j < method.params_terms.size(); j++) {
                        cout << " " << method.params_terms[j];
                    }
                } else {
                    cout << " void";
                }
                cout << ")': " << method.return_term;
                if (i != it->methods.size() - 1) {
                    cout << ", ";
                }
                cout << endl;
            }
            cout << "};" << endl;
        }
    }
}
