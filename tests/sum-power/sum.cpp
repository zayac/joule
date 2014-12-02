#include "cal.h"

message power_int(int v);
message power_double(double v);

variant sum_int(int a, int b) {
  power_int(a + b);
}

variant sum_double(double a, double b) {
  power_int(a + b);
}
