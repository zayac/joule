#pragma once

#include <string>
#include <map>
#include <set>

namespace shell {

class InterfaceMap {
public:
    void add_relation(int ch, std::string var);
    void print_cmap() const;
    void print_vmap() const;
    const std::multimap<int, std::string>& getCMap() const { return cmap; }
    const std::multimap<std::string, int>& getVMap() const { return vmap; }
    void add_variant_substitution(int ch, const std::string &from, const std::string &to);
    void add_record_substitution(int ch, const std::string &from_variant, const std::string &from_record, const std::string &to);
    const std::map<std::pair<int, std::string>, std::string>& getVariantSubstitutions() const { return variant_substitutions; }
    const std::map<std::tuple<int, std::string, std::string>, std::string>& getRecordSubstitutions() const { return record_substitutions; }
private:
    std::multimap<int, std::string> cmap;
    std::multimap<std::string, int> vmap;
    std::map<std::pair<int, std::string>, std::string> variant_substitutions;
    std::map<std::tuple<int, std::string, std::string>, std::string> record_substitutions;

    template<class T>
    void print_map(const T& map) const;
};

class Shell {
public:
    void parse(const std::string file_name);

    const InterfaceMap& getOutInterface() const { return out_interface; }

    const std::set<int>& getChannels() const { return channels; }
private:
    bool isChannel(const std::string &input) const;
    int toChannel(const std::string &input) const;
    bool isVariant(const std::string &input) const;
    std::string toVariant(const std::string &input) const;
    static bool validChar(char c);
    static void trim(std::string &s);

    std::set<int> channels;
    InterfaceMap out_interface;
};

}
