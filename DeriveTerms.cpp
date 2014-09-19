#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "TermConverter.h"

#include <iostream>

using namespace std;

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

DeclarationMatcher ClassMatcher = recordDecl(unless(isImplicit())).bind("recDecl");

TermConverter TC;

class ClassPrinter : public MatchFinder::MatchCallback {
public:

    virtual void run(const MatchFinder::MatchResult &Result) {
        ASTContext *Context = Result.Context;
        if (const CXXRecordDecl *RD = Result.Nodes.getNodeAs<clang::CXXRecordDecl>("recDecl")) {
            // We do not want to parse header files!
            if (!RD || !Context->getSourceManager().isInMainFile(RD->getLocation()))
                return;
            if (!RD->getParent()->isRecord()) {
                TC.parseRecord(RD);
            }
        }
    }
};

int main(int argc, const char **argv) {
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());

    ClassPrinter Printer;
    MatchFinder Finder;
    Finder.addMatcher(ClassMatcher, &Printer);

    Tool.run(newFrontendActionFactory(&Finder).get());
    TC.printClassTerms();
    return 0;
}
