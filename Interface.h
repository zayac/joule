#pragma once

#include <map>
#include <string>
#include <iostream>

#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace llvm;
using namespace clang;

namespace interface {

typedef std::map<std::string, std::string> message;
typedef std::map<std::string, message> volleys;

void printMessage(const message &msg);

bool isValidType(const QualType& ty);
std::string getTypeAsString(const QualType& ty, bool quotation_marks = true);

class Interface {

	std::map<std::string, volleys> output_variants;
	std::map<std::string, message> input_variants;
public:
	void addInputDeclaration(std::string variant_name, const message &msg);
	void addOutputVolley(std::string variant_name, const std::pair<std::string, message> &v);

	void printInputInterface() const;
	void printOutputInterface() const;
};

}