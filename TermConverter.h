#pragma once

using namespace std;

using namespace llvm;
using namespace clang;

class TermConverter {
public:
    enum Access {Public_Access, Private_Access};
    struct MethodDecl {
        enum Access access;
        string name;
        string return_term;
        vector<string> params_terms;
        bool is_const = false;
    };
    struct FieldDecl {
        enum Access access;
        bool is_volatile = false;
        string name;
        string type;
        bool is_const = false;
    };
    struct ClassDecl {
        enum Access access;
        string name;
        vector<ClassDecl> classes;
        vector<FieldDecl> fields;
        vector<MethodDecl> methods;
    };
	ClassDecl parseRecord(const CXXRecordDecl *RD);
	void printClassTerms();
    void printClassTerm(const ClassDecl& c, int depth);
    void addClass(ClassDecl new_class);
private:
    vector<ClassDecl> cl;
    string getTypeAsTerm(const QualType& ty, bool quotation_marks) const;
    bool isValidTerm(const QualType& ty) const;
};