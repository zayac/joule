#pragma once

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "Interface.h"

using namespace llvm;
using namespace clang;

namespace component_analysis
{
	interface::message getMessageFromFunctionDecl(const FunctionDecl* FD);
	std::pair<std::string, interface::message> getVolleyFromCallExpr(const CallExpr* CE);
}