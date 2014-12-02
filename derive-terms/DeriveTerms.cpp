#include <fstream>
#include "DeriveTerms.h"

std::unique_ptr<term::Term> ComponentAnalyser::getDeclFromFunctionDecl(const FunctionDecl* FD, enum interface::InterfaceType it) {
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
    return std::unique_ptr<term::Term>(new term::Record(struct_term, function_name));
}

std::pair<std::string, std::unique_ptr<term::Term>> ComponentAnalyser::getDeclFromCallExpr(const ASTContext *Context, const CallExpr* CE, enum interface::InterfaceType it) {
    const FunctionDecl* FD = CE->getDirectCallee();
    std::string message_name = FD->getNameAsString();
    if (output_interface_flags.find(message_name) == output_interface_flags.end()) {
        std::set<std::string> s{input_interface_flags[function_name]};
        output_interface_flags[message_name] = s;
    } else
        output_interface_flags[message_name].insert(input_interface_flags[function_name]); 

    return make_pair(message_name, getDeclFromFunctionDecl(FD, it));
}

std::unique_ptr<term::Term> merge_records(const std::unique_ptr<term::Term> &acc, const std::unique_ptr<term::Term> &rec) {
    term::Record* casted_acc = static_cast<term::Record*>(acc.get());
    term::Record* casted_rec = static_cast<term::Record*>(rec.get());
    return std::unique_ptr<term::Term>(new term::Record(casted_acc->head, casted_acc->tail + "+" + casted_rec->tail));
}

void ComponentAnalyser::run(const MatchFinder::MatchResult &Result) {
    ASTContext *Context = Result.Context;
    if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("componentVariantDecl")) {
        function_name = FD->getNameAsString();
        file_name = getFileNamePrefix(Context, FD->getLocStart());
        file_name_with_path = getFileNamePrefixWithPath(Context, FD->getLocStart());
        input_interface_flags[function_name] = "f_" + file_name + "_" + function_name;
        input_interface[function_name] = getDeclFromFunctionDecl(FD, interface::TInputInterface);
    } else if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("messageCall")) {
        if (function_name.empty()) {
            std::cerr << "expression that sends a message is used in unknown context" << std::endl;
            return;
        }
       // output_interface.insert(getDeclFromCallExpr(Context, CE, interface::TOutputInterface));
        
       std::pair<std::string, std::unique_ptr<term::Term>> p = getDeclFromCallExpr(Context, CE, interface::TOutputInterface);
        if (output_interface.find(p.first) != output_interface.end()) {
            output_interface[p.first] = merge_records(output_interface[p.first], p.second);
        } else {
            output_interface.insert(getDeclFromCallExpr(Context, CE, interface::TOutputInterface));
        }
    }
}

int main(int argc, const char **argv) {
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
    ComponentAnalyser analyser;
    MatchFinder Finder;
    Finder.addMatcher(ComponentMatcher, &analyser);
    Finder.addMatcher(MessageCallMatcher, &analyser);

    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());
    Tool.run(newFrontendActionFactory(&Finder).get());

    std::ofstream ofile;
    ofile.open(analyser.file_name_with_path + ".terms");

    ofile << "(:";
    for (auto it = analyser.input_interface.begin(); it != analyser.input_interface.end(); ++it) {
        if (it != analyser.input_interface.begin())
            ofile << ", ";
        std::string flag = analyser.input_interface_flags[it->first];
        ofile << it->first << "(" << flag << "): " << interface::toString(it->second);
    }
    ofile << "| $f_" << analyser.file_name;
    ofile << " :)" << std::endl << std::endl;

    ofile << "(:";
    for (auto it = analyser.output_interface.begin(); it != analyser.output_interface.end(); ++it) {
        if (it != analyser.output_interface.begin())
            ofile << ", ";
        ofile << it->first << "(";
        std::set<std::string> s = analyser.output_interface_flags[it->first];
        if (s.size() > 1)
            ofile << "or ";
        for (auto sit = s.begin(); sit != s.end(); ++sit) {
            if (sit != s.begin())
                ofile << " ";
            ofile << *sit;
        }
        ofile << "): " << interface::toString(it->second);
    }
    ofile << "| $f_" << analyser.file_name;
    ofile << " :)";
    return 0;
}
