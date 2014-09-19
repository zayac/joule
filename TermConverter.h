#pragma once

using namespace std;

using namespace llvm;
using namespace clang;

class TermConverter {
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
        string name;
        string type;
        bool is_const = false;
    };
    struct ClassDecl {
    	string name;
        vector<ClassDecl> classes;
        vector<FieldDecl> fields;
    	vector<MethodDecl> methods;
    };
    vector<ClassDecl> cl;
    string getTypeAsTerm(const QualType& ty) const;
    bool isValidTerm(const QualType& ty) const;

public:
	void parseRecord(const CXXRecordDecl *RD);
	void printClassTerms();
    void printClassTerm(const ClassDecl& c, int depth);
};