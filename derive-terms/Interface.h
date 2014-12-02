#pragma once

#include <map>
#include <string>
#include <iostream>
#include <vector>
#include <set>

#include "clang/ASTMatchers/ASTMatchFinder.h"

#include "CalTerm.h"

using namespace llvm;
using namespace clang;

namespace interface {

enum InterfaceType {
	TInputInterface,
	TOutputInterface
};

std::unique_ptr<term::Term> classDeclToTerm(const CXXRecordDecl *RD, enum InterfaceType it);

bool isValidType(const QualType& ty);

std::unique_ptr<term::Term> typeToTerm(const QualType& ty, enum InterfaceType it);

std::string toString(const std::unique_ptr<term::Term> &term);

/*
class Interface {
	std::unique_ptr<term::Term> output_interface;
	std::unique_ptr<term::Term> input_interface;
public:
	void addInputVariant(std::string variant_name, std::unique_ptr<term::Term>);
};
*/
}
