#pragma once

#include <map>
#include <string>
#include <iostream>
#include <vector>
#include <set>

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"

#include "CalTerm.h"

using namespace llvm;
using namespace clang;

//extern std::map<std::string, std::unique_ptr<term::Term>> class_declarations;
extern std::set<std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>, term::TermComparator> constraints;
extern std::set<std::string> cached_classes;
//extern std::map<std::string, const QualType&> class_storage;

extern Rewriter TheRewriter;
extern SourceManager* TheSourceMgr;

namespace interface {

enum InterfaceType {
	TInputInterface,
	TOutputInterface
};

std::unique_ptr<term::Term> classDeclToTerm(const CXXRecordDecl *RD, enum InterfaceType it);

bool isValidType(const QualType& ty);

std::unique_ptr<term::Term> typeToTerm(const QualType& ty, enum InterfaceType it);

/*
class Interface {
	std::unique_ptr<term::Term> output_interface;
	std::unique_ptr<term::Term> input_interface;
public:
	void addInputVariant(std::string variant_name, std::unique_ptr<term::Term>);
};
*/
}
