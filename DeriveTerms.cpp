#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "ComponentAnalysis.h"
#include "Interface.h"

using namespace clang::tooling;
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...");

DeclarationMatcher ComponentMatcher = functionDecl(isDefinition(), returns(asString("variant"))).bind("componentVariantDecl");
StatementMatcher MessageCallMatcher = callExpr(hasDeclaration(functionDecl(returns(asString("message"))))).bind("messageCall");

class ComponentAnalyser : public MatchFinder::MatchCallback {
    std::string function_name = "";
    interface::Interface comp_int;
public:

    virtual void printInputInterface() {
        comp_int.printInputInterface(0);
    }

    virtual void printOutputInterface() {
        comp_int.printOutputInterface(0);
    }

    virtual void run(const MatchFinder::MatchResult &Result) {
        ASTContext *Context = Result.Context;
        if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("componentVariantDecl")) {
            function_name = FD->getNameAsString();
            interface::message msg = component_analysis::getMessageFromFunctionDecl(FD);
            comp_int.addInputDeclaration(function_name, msg);
        } else if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("messageCall")) {
            if (function_name.empty()) {
                std::cerr << "expression that sends a message is used in unknown context" << std::endl;
                return;
            }
            std::pair<std::string, interface::message> volley = component_analysis::getVolleyFromCallExpr(CE);
            comp_int.addOutputVolley(function_name, volley);
        }
    }
};

int main(int argc, const char **argv) {
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());

    ComponentAnalyser analyser;
    MatchFinder Finder;
    Finder.addMatcher(ComponentMatcher, &analyser);
    Finder.addMatcher(MessageCallMatcher, &analyser);

    Tool.run(newFrontendActionFactory(&Finder).get());
    std::cout << "Input interface: " << std::endl;
    analyser.printInputInterface();
    std::cout << std::endl << "Output interface: " << std::endl;
    analyser.printOutputInterface();
    return 0;
}
