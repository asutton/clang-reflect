// RUN: echo

#include <cppx/meta>

using namespace cppx::meta;

template<typename T>
void foo() {
  constexpr auto type = reflexpr(T);
  static_assert(std::is_same_v<typename(type), T>);

  typename(type) x = 0;
  static_assert(std::is_same_v<typename(type), int>);
}

struct S { };

int main(int argc, const char* argv[]) {
  int local;
  constexpr variable_info v = reflexpr(local);
  constexpr auto t = v.type();

  typename(t) x = 0; // int x = 0;
  static_assert(std::is_same_v<decltype(x), int>);

  typename(v) y = 0; // int y = 0;
  static_assert(std::is_same_v<decltype(y), int>);


  static_assert(std::is_same_v<typename(t), int>);

  typename(reflexpr(S)) s0;

  foo<int>();
}
