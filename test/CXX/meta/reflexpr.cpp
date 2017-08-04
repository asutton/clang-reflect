// RUN: echo

#include <cppx/meta>

using namespace cppx::meta;

struct S {
  void f() { }
  int m;
};

union U {
  int m;
};

enum E { X };

namespace N { }

int global = 0;

int main(int argc, const char* argv[]) {
  int local = 0;

  static_assert(reflexpr(void).index() == void_type);
  static_assert(reflexpr(char).index() == character_type);
  static_assert(reflexpr(bool).index() == integral_type);
  static_assert(reflexpr(int).index() == integral_type);
  static_assert(reflexpr(float).index() == floating_point_type);

  static_assert(void_type_info(reflexpr(void)));
  static_assert(character_type_info(reflexpr(char)));
  static_assert(integral_type_info(reflexpr(int)));
  static_assert(floating_point_type_info(reflexpr(float)));
  static_assert(reference_type_info(reflexpr(int&)));
  
  // FIXME: This doesn't parse. It should.
  // static_assert(function_type_info(reflexpr(void())));
  
  static_assert(pointer_type_info(reflexpr(int*)));
  static_assert(array_type_info(reflexpr(int[3])));

  static_assert(reflexpr(local).index() == variable_decl);
  static_assert(reflexpr(global).index() == variable_decl);
  static_assert(reflexpr(main).index() == function_decl);
  static_assert(reflexpr(argc).index() == parameter_decl);
  static_assert(reflexpr(S).index() == class_decl);
  static_assert(reflexpr(U).index() == union_decl);
  static_assert(reflexpr(S::m).index() == member_variable_decl);
  static_assert(reflexpr(S::f).index() == member_function_decl);
  static_assert(reflexpr(U::m).index() == member_variable_decl);
  static_assert(reflexpr(E).index() == enum_decl);
  static_assert(reflexpr(X).index() == enumerator_decl);
  static_assert(reflexpr(N).index() == namespace_decl);

  static_assert(variable_info(reflexpr(local)));
  static_assert(function_info(reflexpr(main)));
  static_assert(parameter_info(reflexpr(argc)));
  static_assert(class_info(reflexpr(S)));
  static_assert(union_info(reflexpr(U)));
  static_assert(member_variable_info(reflexpr(S::m)));
  static_assert(member_function_info(reflexpr(S::f)));
  static_assert(enum_info(reflexpr(E)));
  static_assert(enumerator_info(reflexpr(X)));
  static_assert(namespace_info(reflexpr(N)));
} 
