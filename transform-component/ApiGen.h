#include <string>
#include <sstream>
#include <map>
#include <set>
#include <vector>
#include <regex>

extern std::map<std::string, std::set<std::string>> output_interfaces_names;

struct VariantInfo {
    int channel;
    std::string name;
    std::vector<std::string> params;
    bool operator<(VariantInfo other) const {return name < other.name; }
};

struct SalvoInfo {
    std::string name;
    std::vector<std::string> param_names;
    std::vector<std::string> param_types;
    std::set<std::string> flags;
};

std::pair<int, std::string> slice(const std::string &s) {
    std::regex channel("_([[:digit:]]+)_(.*)");
    std::smatch match;
    std::string::size_type sz;
    if (std::regex_search(s.begin(), s.end(), match, channel)) {
        return make_pair(std::stoi(match[1], &sz), match[2]);
    } else {
        return make_pair(-1, s);
    }
}

class ApiGen {
    std::map<int, std::set<VariantInfo>> variants;
    std::map<std::string, SalvoInfo> salvos;
public:
    void addVariantInfo(int channel, VariantInfo vi) {
        if (variants.find(channel) == variants.end())
            variants.insert(make_pair(channel, std::set<VariantInfo>()));
        variants[channel].insert(vi);
    }

    void addSalvoInfo(std::string name, SalvoInfo si) {
        if (salvos.find(name) == salvos.end()) {
            salvos.insert(make_pair(name, si));
        } else {
            for (const std::string& flag : si.flags) {
                salvos.find(name)->second.flags.insert(flag);
            }
        }
    }

    std::string genInputApi(std::string prefix, std::ostream& header_file) {
        std::ostringstream ss;
        for (auto cit = variants.begin(); cit != variants.end(); ++cit) {
            ss << "\nvoid input_" << cit->first << "(Message&& _msg) {\n";
            for (auto it = cit->second.begin(); it != cit->second.end(); ++it) {
                ss << "#ifndef f_" << prefix << "__" << cit->first << "_" << it->name << "\n"
                        << "\tif (_msg.getType() == \"" << it->name << "\") {\n";
                if (it->params.empty()) {
                    ss << "\t\t_" << cit->first << "_" << it->name << "();\n";
                } else {
                    ss << "\t\tcereal::BinaryInputArchive iarchive(_msg.ss);\n"
                            << "\t\tstd::tuple<";
                    for (auto pit = it->params.begin(); pit != it->params.end(); ++pit) {
                        if (pit != it->params.begin())
                            ss << ", ";
                        ss << *pit;
                    }
                    ss << " " << prefix << "_DOWN__" << cit->first << "_" << it->name << "_types > _data;\n";
                    header_file << "#define " << prefix << "_DOWN__" << cit->first << "_" << it->name << "_types\n";

                    ss << "\t\tiarchive(";
                    int i = 0;
                    for (auto pit = it->params.begin(); pit != it->params.end(); ++pit) {
                        if (pit != it->params.begin())
                            ss << ", ";
                        ss << "std::get<" << i << ">(_data)";
                        ++i;
                    }
                    ss << " " << prefix << "_DOWN__" << cit->first << "_" << it->name << "_tuple_get );\n";
                    header_file << "#define " << prefix << "_DOWN__" << cit->first << "_" << it->name << "_tuple_get\n";

                    ss << "\t\t_" << cit->first << "_" << it->name << "(";
                    i = 0;
                    for (auto pit = it->params.begin(); pit != it->params.end(); ++pit) {
                        if (pit != it->params.begin())
                            ss << ", ";
                        ss << "std::get<" << i << ">(_data)";
                        ++i;
                    }
                    ss << " " << prefix << "_DOWN__" << cit->first << "_" << it->name << "_tuple_get );\n";
                }
                ss << "\t\treturn;\n"
                        << "\t}\n"
                        << "#endif\n";
            }
            ss << "}\n";
        }
        return ss.str();
    }

    std::string genOutputApi(std::string prefix, std::ostream& header_file) {
        std::ostringstream ss;
        for (auto cit = salvos.begin(); cit != salvos.end(); ++cit) {
            // tail
            std::set<std::string> set = output_interfaces_names[cit->first];
            std::string tail_name = "";
            // string concatenation
            for (auto it = set.begin(); it != set.end(); ++it) {
                tail_name += *it;
            }

            ss << "\n#if ";
            for (auto it = cit->second.flags.begin(); it != cit->second.flags.end(); ++it) {
                if (it != cit->second.flags.begin())
                    ss << " || ";
                ss << "!defined(f_" << prefix << "_" << *it << ")";
            }
            ss << "\n";
            ss << "message " << cit->first << "(";
            for (int i = 0; i < cit->second.param_names.size(); ++i) {
                if (i != 0)
                    ss << ", ";
                ss << cit->second.param_types[i] << " ";
                ss << cit->second.param_names[i];
            }

            std::pair<int, std::string> p = slice(cit->first);

            ss << " " << prefix << "_DOWN_" << tail_name << "_" << p.second << "_decl) {\n";
            ss << "\tfor (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({";
            ss << " " << prefix << "_DOWN_" << cit->first << "_ochannels ";
            header_file << "#define " << prefix << "_DOWN_" << cit->first << "_ochannels\n";

            ss << "})) {\n";
            ss << "\t\tMessage _msg;\n";
            ss << "\t\tcereal::BinaryOutputArchive oarchive(_msg.ss);\n";
            ss << "\t\toarchive(";
            for (int i = 0; i < cit->second.param_names.size(); ++i) {
                if (i != 0)
                    ss << ", ";
                ss << cit->second.param_names[i];
            }
            ss << " " << prefix << "_DOWN_" << tail_name << "_" << p.second << "_use);\n";
            ss << "\t\t_msg.setType(_p.second);\n";
            ss << "\t\toutput(_p.first, std::move(_msg));\n";
            ss << "\t}\n";
            ss << "}\n";
            ss << "#endif\n";
        }
        return ss.str();
    }
};



/*
void input_1(Message&& msg) {
#ifndef f_sum__1_sum_int
    if (msg.getType() == "sum_int") {
        cereal::BinaryInputArchive iarchive(msg.ss);
        std::tuple<int, int, int> data;
        iarchive(std::get<0>(data), std::get<1>(data), std::get<2>(data));
        _1_sum_int(std::get<0>(data), std::get<1>(data), std::get<2>(data));
        return;
    }
#endif
#ifndef f_sum__1_sum_double
    if (msg.getType() == "sum_double") {
        cereal::BinaryInputArchive iarchive(msg.ss);
        std::tuple<double, double, double> data;
        iarchive(data[0], data[1], data[2]);
        _1_sum_double(data[0], data[1], data[2]);
        return;
    }
#endif
}
*/