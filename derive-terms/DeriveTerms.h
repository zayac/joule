#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
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

//std::map<std::string, std::unique_ptr<term::Term>> class_declarations;
std::set<std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>, term::TermComparator> constraints;

DeclarationMatcher ComponentMatcher =
    functionDecl(isDefinition(), returns(asString("variant"))).bind("componentVariantDecl");
StatementMatcher MessageCallMatcher =
    callExpr(hasDeclaration(functionDecl(returns(asString("message"))))).bind("messageCall");

inline std::string getFileName(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    return Context->getSourceManager().getFilename(SpellingLoc);
}

inline std::string getFileNamePrefixWithPath(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    std::string full_path = getFileName(Context, SpellingLoc);
    full_path = full_path.substr(0, full_path.find('.', full_path.find_last_of('/')));
    return full_path;
}

inline std::string getFileNamePrefix(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    std::string full_path = getFileName(Context, SpellingLoc);
    full_path = full_path.substr(full_path.find_last_of('/')+1);
    full_path = full_path.substr(0, full_path.find('.'));
    return full_path;
}

class ComponentAnalyser : public MatchFinder::MatchCallback {
    std::string function_name;

    std::unique_ptr<term::Term> getDeclFromFunctionDecl(const FunctionDecl* FD, enum interface::InterfaceType it);
    std::pair<std::string, std::unique_ptr<term::Term>> getDeclFromCallExpr(const ASTContext *Context, const CallExpr* CE, enum interface::InterfaceType it);
public:
    std::string file_name;
    std::string file_name_with_path;

    std::map<std::string, std::unique_ptr<term::Term>> output_interface;
    std::map<std::string, std::unique_ptr<term::Term>> input_interface;
    std::map<std::string, std::string> input_interface_flags;
    std::map<std::string, std::set<std::string>> output_interface_flags;

    virtual void run(const MatchFinder::MatchResult &Result);
};
