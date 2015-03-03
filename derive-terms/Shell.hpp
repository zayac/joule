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
private:
    std::multimap<int, std::string> cmap;
    std::multimap<std::string, int> vmap;

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
