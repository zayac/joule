#pragma once

#pragma clang system_header
#pragma GCC system_header

#include <vector>
#include <string>
#include <sstream>
#include <cereal/archives/binary.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/string.hpp>

typedef void variant;
typedef void message;
typedef void variant_message;

namespace global {

class GlobalObject {
    int a;
public:
    GlobalObject() : a(5) {}
};

}

class Message {
    std::string type;
public:
    std::stringstream ss;

    std::string getType() const { return type; }
    void setType(const std::string& s) { type = s; }
};

void output(int channel, Message&&);


