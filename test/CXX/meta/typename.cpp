// RUN: %clang -std=c++1z %s

#include <cppx/meta>

using namespace cppx;

// template<typename T>
// void foo() {
//   constexpr auto type = reflexpr(T);
//   static_assert(std::is_same_v<typename(type), T>);

//   typename(type) x = 0;
//   static_assert(std::is_same_v<typename(type), int>);
// }

struct S { };

constexpr int test() {
  int local = 0;

  {
    constexpr meta::object x = reflexpr(local);
    constexpr meta::object t = type(x);
    meta::compiler.print(t);
    assert(t == reflexpr(int));
    static_assert(std::is_same_v<typename(t), int>);
  }

  {
    S s;
    constexpr meta::object x = reflexpr(s);
    constexpr meta::object t = type(x);
    meta::compiler.print(t);
    assert(t == reflexpr(S));
    static_assert(std::is_same_v<typename(t), S>);
  }

  return 0;
}

constexpr meta::object get_type()
{
  return reflexpr(S);
}

typename(get_type()) global;

int main(int argc, const char* argv[]) {
  constexpr int n = test();
  
  constexpr int k = (meta::compiler.print(reflexpr(global)), 0);
}
