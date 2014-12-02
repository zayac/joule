#include "cal.h"

message message_foo(int a);
message message_bar(int b);
message message_baz(int c);

variant variant_foo (int a) {
  message_foo(a);
  message_bar(a);
}

variant variant_bar (int a) {
  message_bar(a);
}

variant variant_baz (int a) {
    message_bar(a);
}
