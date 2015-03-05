#include "TransformComponent.h"
#include <iostream>
#include <random>
#include <regex>

static std::default_random_engine random_engine;

void FlowInheritanceHandler::enableCalVariables(FileID fid) {
    SourceLocation sl = Rewrite.getSourceMgr().getLocForStartOfFile(fid);
    Rewrite.InsertTextBefore(sl, "#include \"cal.h\"\n"
                                 "#include \"" + short_file_name + "_" + macro_prefix + "variables.h\"\n");
    header_file.open(directory_path + "/" + short_file_name + "_" + macro_prefix + "variables.h", std::fstream::out);
}

inline std::string getCallExprName(const CallExpr* CE) {
    const FunctionDecl* FD = CE->getDirectCallee();
    return FD->getNameAsString();
}

void genDeclsForCallExprs(Rewriter &Rewrite) {
    for (const auto& el : output_interfaces_names) {
        std::string tail_name = "";
        // string concatenation
        for (auto it = el.second.begin(); it != el.second.end(); ++it) {
            //if (it != el.second.begin())
                //tail_name += '_';
            tail_name += *it;
        }
        for (const CallExpr* exp : output_interfaces_calls[el.first]) {
            const Expr *e = exp->getArg(exp->getNumArgs() - 1);
            Rewrite.InsertTextAfterToken(e->getLocEnd(), " " + short_file_name + "_DOWN_" + tail_name + "_use");
        }
        for (const FunctionDecl* decl : output_interfaces_decls[el.first]) {
            // tail variables
            const ParmVarDecl *p = decl->getParamDecl(decl->getNumParams() - 1);
            Rewrite.InsertTextAfterToken(p->getLocEnd(), " " + short_file_name + "_DOWN_" + tail_name + "_decl");
        }

        header_file << "#define " << short_file_name << "_DOWN_" << tail_name << "_decl" << std::endl;
        header_file << "#define " << short_file_name << "_DOWN_" << tail_name << "_use" << std::endl;
    }
}

void genVolleysPredicates(Rewriter &Rewrite) {
    for (const auto& el : output_interfaces_decls) {
        for (const FunctionDecl* decl : el.second) {
            std::string cond = "";
            bool is_first = true;
            for (const auto& name : output_interfaces_names[el.first]) {
                if (!is_first) {
                    cond += " || ";
                } else
                    is_first = false;
                cond += "!defined(f_" + name + ")";
            }
            if (!is_first) {
                Rewrite.InsertTextBefore(decl->getLocStart(), "\n#if " + cond + "\n");
                Rewrite.InsertText(decl->getLocEnd().getLocWithOffset(2), "\n#endif\n");
                //Rewrite.ReplaceText(decl->getLocEnd(), ")\n#endif\n");
            } else {
                Rewrite.InsertTextBefore(decl->getLocStart(), "\n#if 0\n");
                Rewrite.InsertText(decl->getLocEnd().getLocWithOffset(2), "\n#endif\n");
                //Rewrite.ReplaceText(decl->getLocEnd(), ")\n#endif\n");
            }
        }
    }
}

static std::set<const CXXRecordDecl*> interface_classes;

static void genInterfaceClassMatchers(Rewriter &Rewrite) {
    for (const CXXRecordDecl* crd : interface_classes) {
        Rewrite.InsertTextBefore(crd->getLocStart(), "\n#if !defined(CAL_FI_HIDE_CLASSES)\n");
        Rewrite.InsertTextBefore(crd->getLocStart(),
                "\n#define " + crd->getNameAsString() + " " + short_file_name + "_DOWN_class_"+ crd->getNameAsString() + "\n");

        Rewrite.InsertTextAfter(crd->getLocEnd().getLocWithOffset(2), "\n#endif\n");
    }
}

static void cacheInterfaceClass(const QualType& ty) {
    const QualType cty = ty.getCanonicalType();
    if (cty->isPointerType() || cty->isReferenceType())
        cacheInterfaceClass(cty->getPointeeType());
    else if (cty->isClassType())
        interface_classes.insert(cty->getAsCXXRecordDecl());
}

static void findInterfaceClasses(const FunctionDecl* fd) {
    cacheInterfaceClass(fd->getReturnType());
    for (FunctionDecl::param_const_iterator pit = fd->param_begin(); pit != fd->param_end(); ++pit) {
        cacheInterfaceClass((*pit)->getOriginalType());
    }
}

static bool containsChannel(const std::string &s) {
    std::regex good_variant("_[[:digit:]]+_.*");
    return regex_match(s, good_variant);
}

void FlowInheritanceHandler::run(const MatchFinder::MatchResult &Result) {
    ASTContext *Context = Result.Context;
    const SourceManager& SM = Context->getSourceManager();

    if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("componentVariantDecl")) {
        /* ignore system headers */
        const SourceLocation& SL = FD->getLocation();
        if (SM.isInSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))) ||
            SM.isInExternCSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))))
            return;

        function_name = FD->getNameAsString();
        if (!containsChannel(function_name)) {
            std::cerr << "variant function must contain a channel as a prefix" << std::endl;
            return;
        }

        findInterfaceClasses(FD);

        if (!header_added) {
            enableCalVariables(Rewrite.getSourceMgr().getFileID(FD->getLocation()));
            header_added = true;
        }

        FunctionDecl::param_const_iterator pit = FD->param_end();
        --pit;

        // tail variable
        Rewrite.InsertTextAfterToken((*pit)->getLocation(), " " + short_file_name + "_DOWN_" + FD->getNameAsString() + "_decl");
        header_file << "#define " << short_file_name << "_DOWN_" << FD->getNameAsString() << "_decl" << std::endl;

        Rewrite.InsertTextBefore(FD->getLocStart(),
                  "#ifndef f_"
                + getFileNamePrefix(Context, FD->getLocStart()) + "_"
                + function_name
                + "\n");
        Rewrite.ReplaceText(FD->getLocEnd(), "}\n#endif\n");
    } else if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("messageCall")) {
        /* ignore system headers */
        const SourceLocation& SL = CE->getLocStart();
        if (SM.isInSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))) ||
            SM.isInExternCSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))))
            return;

        if (function_name.empty()) {
            std::cerr << "expression that sends a message is used in the unknown context" << std::endl;
            return;
        }
        std::string call_name = getCallExprName(CE);
        if (output_interfaces_names.find(call_name) == output_interfaces_names.end()) {
            output_interfaces_names[call_name] = {function_name};
            output_interfaces_calls[call_name] = {CE};
        } else {
            output_interfaces_names[call_name].insert(function_name);
            output_interfaces_calls[call_name].insert(CE);
        }
    } else if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("messageDecl")) {
        function_name = FD->getNameAsString();

        findInterfaceClasses(FD);

        if (!header_added) {
            enableCalVariables(Rewrite.getSourceMgr().getFileID(FD->getLocation()));
            //addHeader(FD->getLocStart());
            header_added = true;
        }

        if (output_interfaces_decls.find(function_name) == output_interfaces_decls.end()) {
            output_interfaces_decls[function_name] = {FD};
        } else {
            output_interfaces_decls[function_name].insert(FD);
        }

    }
}

void PrivateDeclsHandler::run(const MatchFinder::MatchResult &Result) {
    ASTContext *Context = Result.Context;
    const SourceManager& SM = Context->getSourceManager();

    if (const NamedDecl *decl = Result.Nodes.getNodeAs<NamedDecl>("privateDecl")) {
        /* ignore system headers */
        const SourceLocation& SL = decl->getLocStart();
        if (SM.isInSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))) ||
            SM.isInExternCSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))))
            return;

        std::string oldName = decl->getNameAsString();
        std::string newName;
        if (privateVariables.find(oldName) == privateVariables.end()) {
            newName = oldName + "_" + gen_random(10);
            privateVariables[oldName] = newName;
        } else
            newName = privateVariables[oldName];
        Rewrite.ReplaceText(decl->getLocation(), oldName.size(), newName);
    } else if (const MemberExpr *expr = Result.Nodes.getNodeAs<MemberExpr>("privateDeclUse")) {
        /* ignore system headers */
        const SourceLocation& SL = expr->getLocStart();
        if (SM.isInSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))) ||
            SM.isInExternCSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))))
            return;

        std::string oldName = expr->getMemberNameInfo().getName().getAsString();
        if (privateVariables.find(oldName) != privateVariables.end()) {
            std::string newName = privateVariables[oldName];
            //if (expr->isArrow())
                Rewrite.ReplaceText(expr->getExprLoc(), oldName.size(), newName);
        }
    } else if (const AccessSpecDecl *accessDecl = Result.Nodes.getNodeAs<AccessSpecDecl>("accessSpecDecl")) {
        /* ignore system headers */
        const SourceLocation& SL = accessDecl->getLocStart();
        if (SM.isInSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))) ||
            SM.isInExternCSystemHeader(SM.getLocForStartOfFile(SM.getFileID(SL))))
            return;

        Rewrite.ReplaceText(accessDecl->getSourceRange(), "public:");
    } else if (const CXXCtorInitializer *ctor = Result.Nodes.getNodeAs<CXXCtorInitializer>("ctorInitializer")) {
        //if (Context->getSourceManager().isInSystemHeader(Context->getSourceManager().getLocForStartOfFile(Context->getSourceManager().getFileID(decl->getSourceRange().getBegin()))) || Context->getSourceManager().isInExternCSystemHeader(Context->getSourceManager().getLocForStartOfFile(Context->getSourceManager().getFileID(decl->getSourceRange().getBegin()))))
            //return;

        FieldDecl* decl = ctor->getMember();
        if (decl == nullptr)
            return;
        if (decl->getAccess() == AS_private) {
            std::string oldName = decl->getDeclName().getAsString();
            std::string newName;
            if (privateVariables.find(oldName) == privateVariables.end()) {
                newName = oldName + "_" + gen_random(10);
                privateVariables[oldName] = newName;
            } else
                newName = privateVariables[oldName];
            Rewrite.ReplaceText(ctor->getSourceLocation(), oldName.size(), newName);
        }
    }
}

std::string PrivateDeclsHandler::gen_random(const int len) const {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
    static std::uniform_int_distribution<int> uniform_dist(0, sizeof(alphanum)-2);

    std::string s(len, 0);
    for (int i = 0; i < len; ++i) {
        s[i] = alphanum[uniform_dist(random_engine)];
    }
    return s;
}

MyASTConsumer::MyASTConsumer(Rewriter &R) : HandlerForPrivateDecls(R, privateVariables),
                                HandlerForFlowInheritance(R) {
    /* Parsing class definition */
    Matcher.addMatcher(namedDecl(isPrivate()).bind("privateDecl"),
        &HandlerForPrivateDecls);
    Matcher.addMatcher(memberExpr(hasDeclaration(namedDecl(isPrivate()))).bind("privateDeclUse"),
        &HandlerForPrivateDecls);
    Matcher.addMatcher(accessSpecDecl().bind("accessSpecDecl"),
        &HandlerForPrivateDecls);
    /* private fields in constructor initializer list */
    Matcher.addMatcher(constructorDecl(forEachConstructorInitializer(ctorInitializer(isWritten()).bind("ctorInitializer"))),
        &HandlerForPrivateDecls);

    /* Function declarations/calls */
    Matcher.addMatcher(functionDecl(isDefinition(), returns(asString("variant"))).bind("componentVariantDecl"),
        &HandlerForFlowInheritance);
    Matcher.addMatcher(callExpr(hasDeclaration(functionDecl(returns(asString("message"))))).bind("messageCall"),
        &HandlerForFlowInheritance);
    Matcher.addMatcher(functionDecl( returns(asString("message"))).bind("messageDecl"),
        &HandlerForFlowInheritance);

}

void MyASTConsumer::HandleTranslationUnit(ASTContext &Context) {
    Matcher.matchAST(Context);
}

void CalInitialTransformation::EndSourceFileAction() {
    genDeclsForCallExprs(TheRewriter);
    genVolleysPredicates(TheRewriter);
    genInterfaceClassMatchers(TheRewriter);

    size_t pos = file_name.find_last_of('.');
    std::string path = file_name.substr(0, pos) + ".transformed" + file_name.substr(pos);
    std::error_code ec;
    llvm::raw_fd_ostream out_file(path.c_str(), ec, llvm::sys::fs::F_None);
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
        .write(out_file);
}

std::unique_ptr<ASTConsumer> CalInitialTransformation::CreateASTConsumer(CompilerInstance &CI,
                                                StringRef file) {
    file_name = file.data();
    size_t start = file_name.find_last_of("/") + 1;
    size_t end = file_name.find_last_of(".");
    short_file_name = file_name.substr(start, end - start);
    directory_path = file_name.substr(0, file_name.find_last_of('/'));
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter);
}

int main(int argc, const char **argv) {
    random_engine = std::default_random_engine(std::random_device{}());

    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);

    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());
    return Tool.run(newFrontendActionFactory<CalInitialTransformation>().get());
}
