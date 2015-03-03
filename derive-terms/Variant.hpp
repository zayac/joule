#pragma once

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include <string>
#include <map>
#include <regex>
#include <set>
#include <iostream>
#include "CalTerm.hpp"

using namespace clang;

struct Variant {
    int channel;
    std::string name;
    std::string flag;
    std::map<std::string, std::unique_ptr<term::Term>> declaration;
    std::map<std::string, std::set<int>> salvos;
    //std::map<std::pair<int, std::string>, std::unique_ptr<term::Term>> salvos;

    /*Variant() = default;
    Variant(Variant&& v);
    Variant& operator=(Variant&& v);*/

    static std::pair<int, std::string> slice(const std::string &s) {
        std::regex channel("_([[:digit:]]+)_(.*)");
        std::smatch match;
        std::string::size_type sz;
        if (std::regex_search(s.begin(), s.end(), match, channel)) {
            return make_pair(std::stoi(match[1], &sz), match[2]);
        } else {
            return make_pair(-1, s);
        }
    }
};