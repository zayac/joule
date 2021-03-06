#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <fstream>
#include <numeric>
#include "ApiGen.h"

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

std::string directory_path;
std::string file_name;
std::string short_file_name;

const std::string macro_prefix = "CAL_FI_";
/* FIXME poor style below */
std::string function_name;
std::fstream header_file;
std::map<std::string, std::set<std::string>> output_interfaces_names;
std::map<std::string, std::unordered_set<const CallExpr*>> output_interfaces_calls;
std::map<std::string, std::unordered_set<const FunctionDecl*>> output_interfaces_decls;

std::string current_class;
std::map<std::string, std::set<std::string>> class_fields;
void addClassField(const std::string &cl, const std::string& field) {
    if (class_fields.find(cl) == class_fields.end())
        class_fields.insert(make_pair(cl, std::set<std::string>()));
    class_fields.find(cl)->second.insert(field);
}


ApiGen apigen;
struct VariantInfo current_variant;
SourceLocation locationtoInsert;

inline std::string getFileName(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    return Context->getSourceManager().getFilename(SpellingLoc);
}

inline std::string getFileNamePath(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    std::string full_path = getFileName(Context, SpellingLoc);
    full_path = full_path.substr(0, full_path.find('/'));
    return full_path;
}

inline std::string getFileNamePrefixWithPath(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    std::string full_path = getFileName(Context, SpellingLoc);
    full_path = full_path.substr(0, full_path.find('.'));
    return full_path;
}

inline std::string getFileNamePrefix(const ASTContext *Context, const SourceLocation &SpellingLoc) {
    std::string full_path = getFileName(Context, SpellingLoc);
    full_path = full_path.substr(full_path.find_last_of('/')+1);
    full_path = full_path.substr(0, full_path.find('.'));
    return full_path;
}


class FlowInheritanceHandler : public MatchFinder::MatchCallback {
public:
    std::string file_name_with_path;

    FlowInheritanceHandler(Rewriter &Rewrite) : Rewrite(Rewrite), header_added(false) {}

    virtual void run(const MatchFinder::MatchResult &Result);
private:
    bool header_added;
    Rewriter &Rewrite;

    void enableCalVariables(FileID fid);
};

class PrivateDeclsHandler : public MatchFinder::MatchCallback {
public:
    PrivateDeclsHandler(Rewriter &Rewrite,
                        std::unordered_map<std::string, std::string> &cache)
                            : Rewrite(Rewrite), privateVariables(cache) {}

    virtual void run(const MatchFinder::MatchResult &Result);

private:
    Rewriter &Rewrite;
    std::unordered_map<std::string, std::string> &privateVariables;

    std::string gen_random(const int len) const;
};

class MyASTConsumer : public ASTConsumer {
public:
    MyASTConsumer(Rewriter &R);
    void HandleTranslationUnit(ASTContext &Context) override;

private:
    PrivateDeclsHandler HandlerForPrivateDecls;
    FlowInheritanceHandler HandlerForFlowInheritance;
    MatchFinder Matcher;
    std::unordered_map<std::string, std::string> privateVariables;
};

class CalInitialTransformation : public ASTFrontendAction {
public:
    CalInitialTransformation() {}
    
    void EndSourceFileAction() override;
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                   StringRef file) override;

private:
    Rewriter TheRewriter;
};

