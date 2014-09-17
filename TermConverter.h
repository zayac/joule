#ifndef TERM_CONVERTER_H
#define TERM_CONVERTER_H

using namespace std;

using namespace llvm;
using namespace clang;

class TermConverter {
    struct MethodDecl {
        string name;
        string return_term;
        vector<string> params_terms;
        bool is_const = false;
    };
    struct FieldDecl {
        string name;
        string type;
        bool is_const = false;
    };
    struct ClassDecl {
    	string name;
        vector<FieldDecl> fields;
    	vector<MethodDecl> methods;
    };
    vector<ClassDecl> cl;
    string getTypeAsTerm(const QualType& ty) const;
    bool isValidTerm(const QualType& ty) const;

public:
	void parseRecord(const CXXRecordDecl *RD);
	void printClassTerms();
};

#endif