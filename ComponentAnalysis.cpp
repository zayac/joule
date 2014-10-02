#include "ComponentAnalysis.h"
#include "Interface.h"

#include <iostream>

#include "clang/Tooling/Tooling.h"
using namespace clang::tooling;

namespace component_analysis
{

interface::message getMessageFromFunctionDecl(const FunctionDecl* FD) {
	interface::message msg;

	for (FunctionDecl::param_const_iterator pit = FD->param_begin(); pit != FD->param_end(); ++pit) {
		clang::Decl *param = *pit;
 		const NamedDecl *namedDecl = dyn_cast<NamedDecl>(param);
	    const ValueDecl *valueDecl = dyn_cast<ValueDecl>(param);

	    if (namedDecl || valueDecl) {
	    	QualType declQT = valueDecl->getType().getCanonicalType();

	    	if (!interface::isValidType(declQT)) {
	    		std::cerr << "type " << declQT.getAsString() << " is not supported in the interface" << std::endl;
	    	}

	    	clang::ASTContext &context = param->getASTContext();
	    	
	    	msg[namedDecl->getNameAsString()] = interface::getTypeAsString(declQT);
	    } else {
	    	std::cerr << "wrong declaration of function " << FD->getNameAsString() << std::endl;
	    }
	}
	return msg;
}

std::pair<std::string, interface::message> getVolleyFromCallExpr(const CallExpr* CE) {
	const FunctionDecl* FD = CE->getDirectCallee();
	std::string name = FD->getNameAsString();
	interface::message msg = getMessageFromFunctionDecl(FD);
	return make_pair(name,msg);
}

}