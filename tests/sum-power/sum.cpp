#include "cal.h"

message salvo(int i);
message salvo2(double d);

variant _1_sum_int(int a, int b) {
  salvo(a + b);
}

variant _1_sum_double(double a, double b) {
  salvo(a + b);
}
