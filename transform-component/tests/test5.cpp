#include "cal.h"

class X {
private:
  const int ap;
  int ad;
  void baz() const;
public:
  volatile int a;
  class Y {
  public:
    int a;
  };
  unsigned long foo(void* const, int, global::GlobalObject);
  void bar() {
    baz();
    ad += ap + 5;
  }
  X();
};

message message_foo(X c, int d, global::GlobalObject e);

variant variant_foo (int a, const int b, global::GlobalObject c) {
  X cl;
  int t = a + b;
  message_foo(cl, b, c);
}
