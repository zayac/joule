#include "Shell.hpp"
#include <assert.h>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

namespace shell {

void InterfaceMap::add_relation(int ch, std::string var) {
    cmap.insert(make_pair(ch, var));
    vmap.insert(make_pair(var, ch));
}

template<class T>
void InterfaceMap::print_map(const T& map) const {
    for (auto it = map.cbegin(); it != map.cend();) {
        std::cout << it->first << " -> ";
        auto range = map.equal_range(it->first);
        for (it = range.first; it != range.second; ++it) {
            if (it != range.first)
                std::cout << ", ";
            std::cout << it->second;
        }
        std::cout << std::endl;
    }
}

void InterfaceMap::print_cmap() const {
    print_map<decltype(cmap)>(cmap);
}

void InterfaceMap::print_vmap() const {
    print_map<decltype(vmap)>(vmap);
}

bool Shell::isChannel(const std::string& input) const {
    return std::all_of(input.begin(), input.end(), ::isdigit);
}

int Shell::toChannel(const std::string &input) const {
    assert(isChannel(input));
    return std::stoul(input);
}

bool Shell::validChar(char c) {
    return isalnum(c) || c == '_';
}

bool Shell::isVariant(const std::string& input) const {
    bool is_alphanum = !isChannel(input)
                    && std::all_of(input.begin(), input.end(), Shell::validChar);

    if (is_alphanum)
        return true;

    if (input.size() < 2 || input[0] != '"' || input[input.size() - 1] != '"')
        return false;

    std::string dequoted_s = input.substr(1, input.size() - 2);
    return std::all_of(dequoted_s.begin(), dequoted_s.end(), Shell::validChar);
}

std::string Shell::toVariant(const std::string& input) const {
    assert(isVariant(input));
    if (input[0] == '"' && input[input.size() - 1] == '"')
        return input.substr(1, input.size() - 2);
    else
        return input;
}

void Shell::trim(std::string &s) {
    s.erase(remove_if(s.begin(), s.end(), isspace), s.end());
}

void Shell::parse(const std::string file_name) {
    std::ifstream file(file_name);
    std::string line;
    while(std::getline(file, line)) {
        size_t pos = 0;
        if ((pos = line.find("->")) != std::string::npos) {
            std::string left = line.substr(0, pos);
            std::string right = line.substr(pos+2);
            trim(left);
            trim(right);

            std::vector<std::string> leftv;
            std::vector<int> rightv;
            std::string token;
            std::istringstream iss(left);

            while(std::getline(iss, token, ',')) {
                trim(token);
                 if (isVariant(token)) {
                     std::string var = toVariant(token);
                     leftv.emplace_back(var);
                     //out_interface.add_relation(ch, var);
                } else {
                    std::cerr << "wrong representation of a variant '" << token << "'" << std::endl;
                    return;
                }
            }

            iss = std::istringstream(right);
            while(std::getline(iss, token, ',')) {
                trim(token);
                 if (isChannel(token)) {
                    int ch = toChannel(token);
                    rightv.emplace_back(ch);
                    //out_interface.add_relation(ch, var);
                } else {
                    std::cerr << "wrong representation of a channel '" << token << "'" << std::endl;
                    return;
                }
            }

            for (auto cit = rightv.begin(); cit != rightv.end(); ++cit) {
                channels.insert(*cit);
                for (auto vit = leftv.begin(); vit != leftv.end(); ++vit) {
                    out_interface.add_relation(*cit, *vit);
                }
            }
        } else {
            std::cerr << "unknown format" << std::endl;
            return;
        }
    }
}

}
