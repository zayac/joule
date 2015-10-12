#include "cal.h"

message message_foo(int c, int d, int e);

variant variant_foo (int, int, int);

variant variant_foo (int a, int b, int c) {
  message_foo(a, b, c);
}

//(: variant_foo: {a: int, b: int, c: int} :) --> (: variant_foo: {message_foo: {c: int, d: int, e: int}} :)