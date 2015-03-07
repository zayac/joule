#include "cal.h"
#include <math.h>

message result(double p);

variant _1_power_double(double v, double base) {
    result(pow(v, base));
}

variant _1_power_int(std::vector<int> v, int base) {
    int i = v[0];
    result(pow(i, base));
}
