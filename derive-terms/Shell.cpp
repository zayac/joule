#include "Shell.hpp"
#include <assert.h>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include <regex>

namespace shell {

void InterfaceMap::add_relation(int ch, std::string var) {
    cmap.insert(make_pair(ch, var));
    vmap.insert(make_pair(var, ch));
}

void InterfaceMap::add_variant_substitution(int ch, const std::string &from, const std::string &to) {
    //std::cout << "variant: " << ch << " " << from << " " << to << std::endl;
    if (from != to)
        variant_substitutions.insert(make_pair(make_pair(ch, from), to));
}

void InterfaceMap::add_record_substitution(int ch, const std::string &from_variant, const std::string &from_record, const std::string &to) {
    //std::cout << "record: " << ch << " " << from_variant << " " << from_record << " " << to << std::endl;
    if (from_record != to)
    record_substitutions.insert(make_pair(make_tuple(ch, from_variant, from_record), to));
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
        std::regex rename_command("([[:digit:]]+)\\[(.*)/(.*)\\]");
        std::smatch match;
        std::string::size_type sz;
        if(std::all_of(line.begin(),line.end(),isspace)) {

        } else if ((pos = line.find("->")) != std::string::npos) {
            std::string left = line.substr(0, pos);
            std::string right = line.substr(pos + 2);
            trim(left);
            trim(right);

            std::vector <std::string> leftv;
            std::vector<int> rightv;
            std::string token;
            std::istringstream iss(left);

            while (std::getline(iss, token, ',')) {
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
            while (std::getline(iss, token, ',')) {
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
        } else if (std::regex_search(line, match, rename_command)) { /* renaming command */
            int rename_channel = std::stoi(match[1], &sz);
            std::string from_s = match[2];
            std::string to_s = match[3];

            std::regex rename_records("(.*)\\{(.*)\\}");
            std::smatch match_left, match_right;
            bool left_found, right_found;
            if ((left_found = std::regex_search(from_s, match_left, rename_records))
                    && (right_found = std::regex_search(to_s, match_right, rename_records))) {
                std::string from_variant = match_left[1];
                std::string to_variant = match_right[1];

                out_interface.add_variant_substitution(rename_channel, from_variant, to_variant);

                std::istringstream ss_left(match_left[2]), ss_right(match_right[2]);
                std::string token_left, token_right;

                while(std::getline(ss_left, token_left, ',') && std::getline(ss_right, token_right, ',')) {
                    out_interface.add_record_substitution(rename_channel, from_variant, token_left, token_right);
                }

            } else if (!left_found && !right_found) {
                out_interface.add_variant_substitution(rename_channel, from_s, to_s);
            } else {
                std::cerr << "unknown renaming pattern" << std::endl;
            }
        } else {
            std::cerr << "unknown format" << std::endl;
            return;
        }
    }
}

}
