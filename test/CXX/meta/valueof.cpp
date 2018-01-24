#include <cppx/meta>
// #include <cppx/compiler>

// #include <iostream>
// #include <typeinfo>

using namespace cppx;

int var = 42;

void foo() { }

struct S {
  int n;
  void f() { }
};

enum E {
  A, B, C
};

template<meta::object X, typename T>
constexpr void test_ref(T& actual) {
  auto& ref = valueof(X);
  assert(&ref == &actual);
}

template<meta::object X, typename C, typename T>
constexpr void test_member(T C::* actual) {
  auto ptr = valueof(X);
  assert(ptr);
}

int main() {
  {
    constexpr meta::object r = reflexpr(var);
    constexpr int& v = valueof(r);
    static_assert(&v == &var);

    constexpr int n1 = (test_ref<reflexpr(var)>(var), 0);
    constexpr int n2 = (test_ref<reflexpr(foo)>(foo), 0);
  }

  {
    constexpr meta::object x1 = reflexpr(S::n);
    constexpr int S::* p1 = valueof(x1);
    static_assert(p1 == &S::n);    

    constexpr meta::object x2 = reflexpr(S::f);
    constexpr auto p2 = valueof(x2);
    static_assert(p2 == &S::f);    
    
    constexpr int n1 = (test_member<reflexpr(S::n)>(&S::n), 0);
    constexpr int n2 = (test_member<reflexpr(S::f)>(&S::f), 0);
  }

  {
    constexpr meta::object x = reflexpr(A);
    constexpr E val = valueof(x);
    static_assert(val == A);
  }
}
