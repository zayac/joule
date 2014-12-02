#include "cal.h"
#include <math.h>

message result(double p);

variant power_double(double v, double base) {
    result(pow(v, base));
}

variant power_int(int v, int base) {
    result(pow(v, base));
}
