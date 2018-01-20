// RUN: %clang -fsyntax-only -std=c++1z %s

#include <cppx/meta>

using namespace cppx;

struct S {
  void f() { }
  int m;
};

immediate void print_members(meta::object x) {
  meta::compiler.print("print members");
  meta::compiler.print(x);
  for (auto m : x)
    meta::compiler.print(m);
}

int main(int argc, const char* argv[]) {
  print_members(reflexpr(S));
} 

