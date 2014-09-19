#include <vector>

using namespace std;

class X {
	const int ap;
	void baz() const;
public:
  int a;
  vector<int> v;
  X * const b;
  class Y {
  	int a;
  };
  unsigned long foo(void * const, vector<int>, int, double);
  void bar() const;
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