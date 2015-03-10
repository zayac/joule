#include "cal.h"
#include <math.h>

class BClass {
private:
    int a;
public:
    int b;
    void foo(global::GlobalObject &t) volatile {
        b = t.b;
    }
};
message result(BClass p);

variant _1_power_double(BClass v) {
    result(v);
}

variant _1_power_int(BClass v) {
    result(v);
}
