#include "cal.h"

message salvo(std::vector<int> i);
message salvo2(double d);

variant _1_sum_int(int a, int b) {
  std::vector<int> v;
  v.push_back(a);
  salvo(v);
}

variant _1_sum_double(double a, double b) {
  std::vector<int> v;
  v.push_back(a);
  salvo(v);
}
