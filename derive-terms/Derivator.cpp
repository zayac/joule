#include "Derivator.hpp"
#include "Interface.hpp"

#include <algorithm>
#include <iostream>
#include <numeric>

namespace derivator {

void Derivator::run(const std::string& _cpp_fname) {
    setFNames(_cpp_fname);
    this->shell.parse(this->shell_fname);
}

void Derivator::setFNames(const std::string& _cpp_fname) {
    size_t start = _cpp_fname.find_last_of("/") + 1;
    size_t end = _cpp_fname.find(".", start);
    std::string dir_path = _cpp_fname.substr(0, _cpp_fname.find_last_of('/'));

    this->short_fname = _cpp_fname.substr(start, end - start);
    this->cpp_fname = _cpp_fname;
    this->shell_fname = dir_path + "/" + short_fname + ".shell";
    this->terms_fname = dir_path + "/" + short_fname + ".terms";
    this->hash_fname = dir_path + "/code-hash";
}

void Derivator::addVariant(int channel, std::string name, Variant&& var) {
    if (this->variants.find(channel) != this->variants.end())
        this->variants.emplace(channel, std::map<std::string, Variant>());
    this->variants[channel].emplace(name, std::move(var));
}

void Derivator::addSalvo(std::string s, std::map<std::string, std::unique_ptr<term::Term>>&& t) {
    this->salvos.emplace(s, std::move(t));
}

void Derivator::addRouting(int channel, RoutedSalvo route) {
    if (this->routing.find(channel) != this->routing.end()) {
        auto& map = this->routing[channel];
        if (map.find(route.name) != map.end()) {
            /* merge sets of flags */
            std::set<std::string> old_s = map[route.name].flags;
            std::set<std::string> new_s = route.flags;
            set_union (old_s.begin(), old_s.end(), new_s.begin(), new_s.end(), inserter(map[route.name].flags, map[route.name].flags.begin()));
        } else {
            this->routing[channel].emplace(route.name, route);
        }
    } else
        this->routing.emplace(channel, std::map<std::string, RoutedSalvo>({{route.name, route}}));
}

void Derivator::printRoutedSalvos() const {
    for (auto it = this->routing.begin(); it != this->routing.end(); ++it) {
        std::cout << it->first << ": ";
        for (auto sit = it->second.begin(); sit != it->second.end(); ++sit) {
            std::cout << sit->first << "(";
            for (const auto& flag : sit->second.flags) {
                std::cout << flag << " ";
            }
            std::cout << ")";
        }
        std::cout << std:: endl;
    }
}

std::set<int> Derivator::getOutChannels() const {
    std::set<int> s;
    for (auto it = routing.begin(); it != routing.end(); ++it)
        s.insert(it->first);
    return s;
}

void Derivator::genOutTermForChannel(std::ostream& ofile, int ch) const {
    std::map<std::pair<int, std::string>, std::string> rename_variants = getShell().getOutInterface().getVariantSubstitutions();
    std::map<std::tuple<int, std::string, std::string>, std::string> rename_records = getShell().getOutInterface().getRecordSubstitutions();

    ofile << "(:";
    auto salvo_it = this->routing.find(ch);
    if (salvo_it != this->routing.end()) {
        for (auto it = salvo_it->second.begin(); it != salvo_it->second.end(); ++it) {
            if (it != salvo_it->second.begin())
                ofile << ", ";

            /* perform variant field rename */
            auto rv_it = rename_variants.find(make_pair(ch, it->first));
            if (rv_it != rename_variants.end())
                ofile << rv_it->second << "(";
            else
                ofile << it->first << "(";

            std::set<std::string> set = it->second.flags;
            if (set.size() > 1)
                ofile << "or ";
            for(auto fit = set.begin(); fit != set.end(); ++fit) {
                if (fit != set.begin())
                    ofile << " ";
                ofile << "f_" << this->short_fname << "_" << *fit;
            }
            ofile << "): ";

            /* generate term */
            auto record_it = this->salvos.find(it->second.name);
            ofile << "{";
            for (auto rit = record_it->second.begin(); rit != record_it->second.end(); ++rit) {
                if (rit != record_it->second.begin())
                    ofile << ", ";

                /* perform record field rename */
                auto rr_it = rename_records.find(make_tuple(ch, it->first, rit->first));
                if (rr_it != rename_records.end())
                    ofile << "\"" << rr_it->second << "\": " + term::toString(rit->second);
                else
                    ofile << "\"" << rit->first << "\": " + term::toString(rit->second);
            }
            if (!set.empty()) {
                std::string tail_var = accumulate(set.begin(), set.end(), std::string(""));
                ofile << "| $_" << tail_var;
            }
            ofile << "}";
        }
    }
    ofile << "| $^" << this->short_fname;
    ofile << ":)";
}

void Derivator::genInTerms(std::ostream& ofile) const {
    ofile << "IN\n";
    for (auto it = this->variants.begin(); it != this->variants.end(); ++it) {
        ofile << "\t" << it->first << ": (: ";
        for (auto vit = it->second.begin(); vit != it->second.end(); ++vit) {
            if (vit != it->second.begin())
                ofile << ", ";
            const std::map<std::string, std::unique_ptr<term::Term>>& record = vit->second.declaration;
            ofile << vit->second.name << "(" << "f_" << this->short_fname << "_" << vit->second.flag << "): ";
            ofile << "{";
            for (auto rit = record.begin(); rit != record.end(); ++rit) {
                if (rit != record.begin())
                    ofile << ", ";
                ofile << "\"" << rit->first << "\": " + term::toString(rit->second);
            }
            ofile << "| $_" << vit->second.name;
            ofile << "}";
        }
        ofile << "| $^" << this->short_fname;
        ofile << ":)\n";
    }
}

void Derivator::genOutTerms(std::ostream& ofile) const {
    ofile << "OUT\n";
    std::set<int> channels = getOutChannels();
    for (int ch : channels) {
        ofile << "\t" << ch << ": ";
        genOutTermForChannel(ofile, ch);
        ofile << "\n";
    }
}

void Derivator::genConstraints(std::ostream& ofile) const {
    std::set<int> channels = getOutChannels();
    std::set<std::string> cache;
    for (int ch : channels) {
        auto salvo_it = this->routing.find(ch);
        if (salvo_it != this->routing.end()) {
            for (auto it = salvo_it->second.begin(); it != salvo_it->second.end(); ++it) {
                std::set<std::string> set = it->second.flags;
                if (set.size() > 1) {
                    std::string tail_var = accumulate(set.begin(), set.end(), std::string(""));
                    if (cache.find(tail_var) == cache.end()) {
                        for (auto fit = set.begin(); fit != set.end(); ++fit) {
                            ofile << "$_" << *fit << " <= $_" << tail_var << ";" << std::endl;
                            //if (fit != set.begin())
                                //ofile << " ";
                        }
                        cache.emplace(tail_var);
                    }
                }
            }
        }
    }
}

void Derivator::replaceAll(std::string &s, const std::string &search, const std::string &replace) const {
    for(size_t pos = 0; ; pos += replace.length()) {
        // Locate the substring to replace
        pos = s.find(search, pos);
        if(pos == std::string::npos) break;
        // Replace by erasing and inserting
        s.erase(pos, search.length());
        s.insert(pos, replace);
    }
}

void Derivator::genCodeHashFile() const {
    std::ofstream ofile;
    ofile.open(this->hash_fname, std::fstream::app);
    for (auto &el : method_body) {
        std::vector<std::string> params = el.second.first;
        std::string body = el.second.second;
        replaceAll(body, "\"", "\\\"");
        replaceAll(body, "\n", "\\n");
        ofile << "\"" << el.first << "\": (";
        for (std::vector<std::string>::iterator it = params.begin(); it != params.end(); ++it) {
            if (it != params.begin())
                ofile << ", ";
            ofile << "\"" << *it << "\"";
        }
        ofile << ") \"" << body << "\"" << std::endl;
    }
}

}