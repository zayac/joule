#pragma once

#include <vector>
#include <string>

typedef void variant;
typedef void message;
typedef void variant_message;

class Message {
    std::string type;
    std::vector<void*> data;
public:
    std::string getType() const { return type; }
    void setType(const std::string& s) { type = s; }

    std::vector<void*> unpack() const { return data; }
    template<class T>
    void pack(const T& element);
};

void output(int channel, Message);

namespace global {

class GlobalObject {
    int a;
public:
    GlobalObject() : a(5) {}
};

}

