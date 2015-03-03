#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include "Derivator.hpp"
#include "Interface.hpp"
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

/* FIXME Would like to avoid global variables */
Rewriter TheRewriter;
SourceManager* TheSourceMgr;
derivator::Derivator D;

/* Variables are referred from 'ComponentAnalyser::run' function */
/* FIXME Would like to get rid of these variables. */
std::unique_ptr<std::string> variant_name; // nullptr
int variant_channel;
Variant current_variant;

/* further constraints */
std::set<std::pair<std::unique_ptr<term::Term>, std::unique_ptr<term::Term>>, term::TermComparator> constraints;
std::set<std::string> cached_classes;
std::map<std::string, std::pair<std::vector<std::string>, std::string>> method_body;

class ComponentAnalyser : public MatchFinder::MatchCallback {
    virtual void run(const MatchFinder::MatchResult &Result);
public:
};

class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer() {
    Matcher.addMatcher(functionDecl(isDefinition(), returns(asString("variant"))).bind("componentVariantDecl"), &HandlerForTerms);
    Matcher.addMatcher(callExpr(hasDeclaration(functionDecl(returns(asString("message"))))).bind("messageCall"), &HandlerForTerms);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    // Run the matchers when we have the whole TU parsed.
    Matcher.matchAST(Context);
  }

private:
    ComponentAnalyser HandlerForTerms;
    MatchFinder Matcher;
};

class MyFrontendAction : public ASTFrontendAction {
public:
    MyFrontendAction() {}

    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
        TheSourceMgr = &CI.getSourceManager();
        TheRewriter.setSourceMgr(*TheSourceMgr, CI.getLangOpts());
        return llvm::make_unique<MyASTConsumer>();
    }
};
