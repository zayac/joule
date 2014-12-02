#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include <unordered_map>

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

class PrivateDeclsHandler : public MatchFinder::MatchCallback {
public:
    PrivateDeclsHandler(Rewriter &Rewrite,
                        std::unordered_map<std::string, std::string> &cache)
                            : Rewrite(Rewrite), privateVariables(cache) {}

    virtual void run(const MatchFinder::MatchResult &Result) {
        ASTContext *Context = Result.Context;
        if (const NamedDecl *decl = Result.Nodes.getNodeAs<NamedDecl>("privateDecl")) {
            std::string oldName = decl->getNameAsString();
            std::string newName;
            if (privateVariables.find(oldName) == privateVariables.end()) {
                newName = oldName + "_" + gen_random(10);
                privateVariables[oldName] = newName;
            } else
                newName = privateVariables[oldName];
            Rewrite.ReplaceText(decl->getLocation(), oldName.size(), newName);
        } else if (const MemberExpr *expr = Result.Nodes.getNodeAs<MemberExpr>("privateDeclUse")) {
            std::string oldName = expr->getMemberNameInfo().getName().getAsString();
            if (privateVariables.find(oldName) != privateVariables.end()) {
                std::string newName = privateVariables[oldName];
                if (expr->isArrow())
                    Rewrite.ReplaceText(expr->getMemberLoc(), oldName.size(), newName);
            }
        } else if (const AccessSpecDecl *accessDecl = Result.Nodes.getNodeAs<AccessSpecDecl>("accessSpecDecl")) {
            Rewrite.ReplaceText(accessDecl->getSourceRange(), "public:");
        }
    }

private:
    Rewriter &Rewrite;
    std::unordered_map<std::string, std::string> &privateVariables;

    std::string gen_random(const int len) const {
        static const char alphanum[] =
            "0123456789"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "abcdefghijklmnopqrstuvwxyz";

        std::string s(len, 0);
        for (int i = 0; i < len; ++i) {
            s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
        }
        return s;
    }
};

class MyASTConsumer : public ASTConsumer {
public:
    MyASTConsumer(Rewriter &R) : HandlerForPrivateDecls(R, privateVariables) {
        Matcher.addMatcher(namedDecl(isPrivate()).bind("privateDecl"),
            &HandlerForPrivateDecls);
        Matcher.addMatcher(memberExpr(hasDeclaration(namedDecl(isPrivate()))).bind("privateDeclUse"),
            &HandlerForPrivateDecls);
        Matcher.addMatcher(accessSpecDecl().bind("accessSpecDecl"),
            &HandlerForPrivateDecls);
    }

    void HandleTranslationUnit(ASTContext &Context) override {
        Matcher.matchAST(Context);
    }

private:
    PrivateDeclsHandler HandlerForPrivateDecls;
    MatchFinder Matcher;
    std::unordered_map<std::string, std::string> privateVariables;
};

class CalInitialTransformation : public ASTFrontendAction {
public:
    CalInitialTransformation() {}
    
    void EndSourceFileAction() override {
        size_t pos = file_name.find_last_of('.');
        std::string path = file_name.substr(0, pos) + ".transformed" + file_name.substr(pos);
        std::error_code ec;
        llvm::raw_fd_ostream out_file(path.c_str(), ec, llvm::sys::fs::F_None);
        TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
            .write(out_file);
    }

    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                   StringRef file) override {
        file_name = file.data();
        TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
        return llvm::make_unique<MyASTConsumer>(TheRewriter);
    }

private:
    std::string file_name;
    Rewriter TheRewriter;
};

int main(int argc, const char **argv) {
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);

    MatchFinder Finder;

    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());
    Tool.run(newFrontendActionFactory<CalInitialTransformation>().get());
    return 0;
}
