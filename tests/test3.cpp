#include "cal.h"

class X {
private:
  const int ap;
  int ad;
	void baz() const;
public:
  volatile int a;
  class Y {
    private:
  	int a;
  };
  unsigned long foo(void* const, int, double);
  void bar() {
    baz();
    ad += ap + 5;
  }
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

message message_foo(global::GlobalObject c, int d, int e);

variant variant_foo (int a, const int b, int c) {
  global::GlobalObject cl;
  int t = a + b + c;
  message_foo(cl, b, c);
}

//(: variant_foo: {a: int, b: int, c: int} :) --> (: variant_foo: {message_foo: {c: int, d: int, e: int}} :)
