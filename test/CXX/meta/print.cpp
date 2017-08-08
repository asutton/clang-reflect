// RUN: echo

#include <cppx/meta>

using namespace cppx::meta;

struct S {
  void f() { }
  int m;
};

immediate void print_members(class_info ci) {
  compiler.print("print members");
  compiler.print(ci);
  auto rng = ci.members();
  compiler.print(std::distance(rng.begin(), rng.end()));
  for(meta_info m : ci.members())
    compiler.print(m);
}

int main(int argc, const char* argv[]) {
  print_members(reflexpr(S));
} 
