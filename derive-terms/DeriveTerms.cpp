#include <fstream>
#include <regex>
#include "DeriveTerms.hpp"
//#include "Interface.hpp"
#include <iostream>
#include <fstream>

void ComponentAnalyser::run(const MatchFinder::MatchResult &Result) {
    ASTContext *Context = Result.Context;
    /* found a function declaration that returns 'variant' */
    if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("componentVariantDecl")) {
        std::string var_name = FD->getNameAsString();
        /* previous variant is processed. Add to collection of variants */
        if (variant_name != nullptr) {
            D.addVariant(variant_channel, *variant_name, std::move(current_variant));
            current_variant = std::move(Variant());
        }
        std::pair<int, std::string> p = Variant::slice(var_name);
        variant_name = std::unique_ptr<std::string>(new std::string(p.second));
        variant_channel = p.first;
        current_variant.name = *variant_name;
        current_variant.flag = /*"f_" + D.getFPrefix() + */ "_" + std::to_string(variant_channel) + "_" + *variant_name;
        current_variant.declaration = interface::getDeclFromFunctionDecl(FD, interface::TInputInterface);
    /* found a call of a function that returns 'message' */
    } else if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("messageCall")) {
        const FunctionDecl* FD = CE->getDirectCallee();
        std::pair<int, std::string> p = Variant::slice(FD->getNameAsString());

        if (p.first != -1) {
            if (current_variant.salvos.find(p.second) == current_variant.salvos.end()) {
                current_variant.salvos[p.second] = {p.first};
            } else {
                current_variant.salvos[p.second].emplace(p.first);
            }
            /* if channel number is provided, send output message to the channel */
            D.addRouting(p.first, {p.second, {current_variant.flag}});
        } else {
            current_variant.salvos[p.second] = std::set<int>();
            /* if channel number is not provided, check whether routing is defined in shell */
            std::multimap<std::string, int> map_ref = D.getShell().getOutInterface().getVMap();
            if (map_ref.find(p.second) != map_ref.end()) {
                auto range = map_ref.equal_range(p.second);
                for (auto it = range.first; it != range.second; ++it) {
                    D.addRouting(it->second, {p.second, {current_variant.flag}});
                }
            } else {
                /* send to all channels */
                std::set<int> channels = D.getShell().getChannels();
                if (channels.empty())
                    channels.emplace(1);
                for (const auto& el : channels) {
                    D.addRouting(el, {p.second, {current_variant.flag}});
                }
            }
        }

        D.addSalvo(p.second, interface::getDeclFromFunctionDecl(FD, interface::TOutputInterface));
    }
}

int main(int argc, const char **argv) {
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);

    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());

    D.run(OptionsParser.getSourcePathList()[0]);

    if (Tool.run(newFrontendActionFactory<MyFrontendAction>().get()))
        return 1;
    // collect last matched variant
    if (variant_name != nullptr) {
        D.addVariant(variant_channel, *variant_name, std::move(current_variant));
    }

    std::ofstream ofile;
    ofile.open(D.getTermsFName());

    /* Printing further constraint */
    ofile << "/*" << std::endl;
    D.genConstraints(ofile);
    for (auto it = constraints.begin(); it != constraints.end(); ++it) {
        ofile << term::toString(it->first) << " <= " << term::toString(it->second) << ";" << std::endl;
    }
    ofile << "*/" << std::endl;

    /* Printing in terms */
    D.genInTerms(ofile);

    /* Printing out terms */
    D.genOutTerms(ofile);
    
    //D.genJsonFile();
    D.genCodeHashFile();
    return 0;
}
