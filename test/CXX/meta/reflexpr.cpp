
namespace Ok {

struct S { };
enum E { e1, e2, e3 };

namespace N {
  namespace M { }
}

void f(int n) {
  (void)reflexpr(f);
  (void)reflexpr(n);
  (void)reflexpr(S);
  (void)reflexpr(E);
  (void)reflexpr(e1);
  (void)reflexpr(N);
  (void)reflexpr(M);
  
  (void)reflexpr(int);
  (void)reflexpr(const int);
  (void)reflexpr(const int&);
  (void)reflexpr(int[]);
  (void)reflexpr(int[5]);
  (void)reflexpr(int******);
}

}

namespace Bad {

void g();
void g(int);

template<typename T> void g2(T);

void f() {
  reflexpr(x); // expected-error {{reflection of undeclared identifier 'x'}}
  reflexpr(g); // expected-error {{reflection of overloaded identifier 'g'}}
  reflexpr(g2); // expected-error {{reflection of overloaded identifier 'g2'}}
}

}
