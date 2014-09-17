#include <vector>

using namespace std;

struct X {
  int a;
  vector<int> v;
  X* const b;
  unsigned long foo(void * const, char, int, double);
  void bar();
};

struct Y {
	void baz();
    void baz() const;
    X* const b;
};

union Z {
  long c;
};

struct Nil {};