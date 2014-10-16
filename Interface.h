#pragma once

#include <map>
#include <string>
#include <iostream>
#include <vector>

#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace llvm;
using namespace clang;

namespace interface {

namespace class_repr {
	enum Access {Public_Access, Private_Access};
	struct MethodDecl {
	    enum Access access;
	    std::string name;
	    std::string return_term;
	    std::vector<std::string> params_terms;
	    bool is_const = false;
	};
	struct FieldDecl {
	    enum Access access;
	    bool is_volatile = false;
	    std::string name;
	    std::string type;
	    bool is_const = false;
	};
	struct ClassDecl {
	    enum Access access;
	    std::string name;
	    std::vector<ClassDecl> classes;
	    std::vector<FieldDecl> fields;
	    std::vector<MethodDecl> methods;
	};
}

typedef std::map<std::string, std::string> message;
typedef std::map<std::string, message> volleys;

class_repr::ClassDecl getClassDecl(const CXXRecordDecl *RD);

void printMessage(const message &msg, unsigned depth);
std::string classDeclToString(const class_repr::ClassDecl& cl, unsigned depth);

bool isValidType(const QualType& ty);
std::string getTypeAsString(const QualType& ty, bool quotation_marks, unsigned depth);

class Interface {

	std::map<std::string, volleys> output_variants;
	std::map<std::string, message> input_variants;
public:
	void addInputDeclaration(std::string variant_name, const message &msg);
	void addOutputVolley(std::string variant_name, const std::pair<std::string, message> &v);

	void printInputInterface(unsigned) const;
	void printOutputInterface(unsigned) const;
};

}