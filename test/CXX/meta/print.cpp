// RUN: %clang -fsyntax-only -std=c++1z %s

#include <cppx/meta>

using namespace cppx;

struct S {
  void f() { }
  int m;
};

constexpr int print_members(meta::object x) {
  meta::compiler.print("print members");
  meta::compiler.print(x);
  for (auto m : x)
    meta::compiler.print(m);
  meta::compiler.print("----------");
  return 0;
}

template<typename T>
constexpr int print_members()
{
  return print_members(reflexpr(T));
}


int main(int argc, const char* argv[]) {
  constexpr int n1 = print_members(reflexpr(S));
  constexpr int n2 = print_members<S>();
} 

