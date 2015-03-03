#pragma once

#include <map>
#include <string>
#include <iostream>
#include <vector>
#include <set>

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"

#include "CalTerm.hpp"

using namespace llvm;
using namespace clang;

extern std::set<std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>, term::TermComparator> constraints;
extern std::set<std::string> cached_classes;
extern std::map<std::string, std::pair<std::vector<std::string>, std::string>> method_body;

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

std::map<std::string, std::unique_ptr<term::Term>> getDeclFromFunctionDecl(const FunctionDecl* FD, enum InterfaceType it);

}

