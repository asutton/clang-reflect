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
struct S2;

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

template<typename T, meta::object X = reflexpr(T)>
T check()
{
  constexpr int dummy = (meta::compiler.print(X), 0);
  typename(X) var = 0;
  return var + 42;
}

typename(get_type()) global;

int main(int argc, const char* argv[]) {
  constexpr int n = test();
  constexpr int k = (meta::compiler.print(reflexpr(global)), 0);

  assert(check<int>() == 42);
  assert(check<const int>() == 42);
}
