#pragma once

#include "Variant.hpp"
#include "Shell.hpp"
#include "RoutedSalvo.hpp"
#include "Interface.hpp"
#include <string>
#include <map>
#include <set>
#include <fstream>

namespace derivator {

class Derivator {
    std::string short_fname;
    std::string cpp_fname;
    std::string shell_fname;
    std::string terms_fname;
    std::string hash_fname;

    shell::Shell shell;

    std::map<int, std::map<std::string, Variant>> variants;
    std::map<std::string, std::map<std::string, std::unique_ptr<term::Term>>> salvos;

    std::map<int, std::map<std::string, RoutedSalvo>> routing;
public:
    void run(const std::string& _cpp_fname);

    void setFNames(const std::string& _cpp_fname);
    void addVariant(int channel, std::string name, Variant&& var);
    void addSalvo(std::string, std::map<std::string, std::unique_ptr<term::Term>>&& t);
    void addRouting(int channel, RoutedSalvo route);
    std::string getShortFName() const { return short_fname; }
    std::string getFPrefix() const { return short_fname; }
    std::string getTermsFName() const { return terms_fname; }
    shell::Shell getShell() const { return shell; }
    std::set<int> getOutChannels() const;
    void printRoutedSalvos() const;
    void genOutTermForChannel(std::ostream& ofile, int ch) const;
    void genInTerms(std::ostream& ofile) const;
    void genOutTerms(std::ostream& ofile) const;
    void genConstraints(std::ostream& ofile) const;
    void replaceAll(std::string &s, const std::string &search, const std::string &replace) const;
    void genCodeHashFile() const;
    //void genJsonFile() const;
    std::map<std::string, std::unique_ptr<term::Term>> getDeclFromFunctionDecl(const FunctionDecl* FD, enum interface::InterfaceType it) const;
};

}
