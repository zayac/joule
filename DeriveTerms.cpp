#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
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

    std::unique_ptr<term::Term> getDeclFromFunctionDecl(const FunctionDecl* FD, enum interface::InterfaceType it) {
        std::map<std::string, std::unique_ptr<term::Term>> struct_term;

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
                struct_term[namedDecl->getNameAsString()] = typeToTerm(declQT, it);
            } else {
                std::cerr << "wrong declaration of function " << FD->getNameAsString() << std::endl;
            }
        }
        return std::unique_ptr<term::Term>(new term::Record(struct_term));
    }

std::pair<std::string, std::unique_ptr<term::Term>> getDeclFromCallExpr(const CallExpr* CE, enum interface::InterfaceType it) {
    const FunctionDecl* FD = CE->getDirectCallee();
    return make_pair(FD->getNameAsString(), getDeclFromFunctionDecl(FD, it));
}

public:
    std::map<std::string, std::unique_ptr<term::Term>> output_interface;
    std::map<std::string, std::unique_ptr<term::Term>> input_interface;

    virtual void run(const MatchFinder::MatchResult &Result) {
        ASTContext *Context = Result.Context;
        if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("componentVariantDecl")) {
            function_name = FD->getNameAsString();
            input_interface[function_name] = getDeclFromFunctionDecl(FD, interface::TInputInterface);
        } else if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("messageCall")) {
            if (function_name.empty()) {
                std::cerr << "expression that sends a message is used in unknown context" << std::endl;
                return;
            }
            output_interface.insert(getDeclFromCallExpr(CE, interface::TOutputInterface));
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
    std::cout << "(:";
    for (auto it = analyser.input_interface.begin(); it != analyser.input_interface.end(); ++it) {
        if (it != analyser.input_interface.begin())
            std::cout << ", ";
        std::cout << it->first << ": " << interface::toString(it->second);
    }
    std::cout << ":)" << std::endl;

    std::cout << "Output interface: " << std::endl;
    std::cout << "{";
    for (auto it = analyser.output_interface.begin(); it != analyser.output_interface.end(); ++it) {
        if (it != analyser.output_interface.begin())
            std::cout << ", ";
        std::cout << it->first << ": " << interface::toString(it->second);
    }
    std::cout << "}";
    return 0;
}
