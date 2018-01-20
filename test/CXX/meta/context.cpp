// RUN: %clang -std=c++1z %s

#include <cppx/meta>
// #include <cppx/compiler>

// #include <iostream>
// #include <typeinfo>

using namespace cppx;

namespace N1 { 
  namespace N2 {
    struct S { };
  }
  int x;
}

immediate int nesting(meta::object d) {
  int n = 0;
  meta::object p = parent(d);
  while (!is_translation_unit(p)) {
    p = parent(p);
    ++n;
  }
  return n;
}

int main(int argc, char* argv[]) {

  constexpr meta::object n1 = reflexpr(N1);
  constexpr meta::object n2 = reflexpr(N1::N2);
  constexpr meta::object s = reflexpr(N1::N2::S);
  constexpr meta::object x = reflexpr(N1::x);

  static_assert(parent(n2) == n1);
  static_assert(parent(s) == n2);
  static_assert(parent(x) == n1);

  constexpr meta::object t1 = parent(n1);
  static_assert(is_translation_unit(t1));

  constexpr meta::object null = parent(t1);
  static_assert(is_null(null));

  static_assert(nesting(s) == 2);
}
