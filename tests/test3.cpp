#include "cal.h"

class X {
	const int ap;
	void baz() const;
public:
  volatile int a;
  class Y {
  	int a;
  };
  unsigned long foo(void * const, int, double);
  void bar() const;
  X();
};

class Y {
public:
	void baz();
    void baz() const;
    X* const b;
};

union Z {
  long c;
};

class Nil {};

message message_foo(X c, int d, int e);

variant variant_foo (int a, const int b, int c) {
  X cl;
  message_foo(cl, b, c);
}

//(: variant_foo: {a: int, b: int, c: int} :) --> (: variant_foo: {message_foo: {c: int, d: int, e: int}} :)
