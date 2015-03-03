#pragma once

typedef void variant;
typedef void message;
typedef void variant_message;

namespace global {

class GlobalObject {
private:
    int a;
public:
    int b;
    GlobalObject() : a(5) {}
};

}

