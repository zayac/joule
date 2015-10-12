#include "cal.h"

class Test {
    public:
        int x;
        int y;
};

class AClass {
private:
    int a;
    int c;
public:
    int b;
    double f();
    AClass(const AClass& cl) {}
    AClass() {
        a = 1;
        b = a;
        c = a + b + 1;
    }

    void foo(global::GlobalObject &t) {
        a = t.b;
    }

    int bar(int a, int b, int c) {
        return a + b + c;
    }
};

message power_int(AClass v);
message power_double(AClass v);

variant _1_sum_int(int a, int b) {
  power_int(AClass());
}

variant _1_sum_double(double a, double b) {
  power_int(AClass());
}
