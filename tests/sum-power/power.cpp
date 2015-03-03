#include "cal.h"
#include <math.h>

message result(double p);

variant _1_power_double(double v, double base) {
    result(pow(v, base));
}

variant _1_power_int(int v, int base) {
    result(pow(v, base));
}
