#include "cal.h"

class X {
private:
  const int ap;
	void baz() const;
public:
  volatile int a;
  class Y {
	private:
  	int a;
  };
  unsigned long foo(void* const, int, double);
  void bar() const;
  X();
};

message message_foo(X c, int d, int e);

variant variant_foo (int a, const int b, int c) {
  X cl;
  message_foo(cl, b, c);
}

