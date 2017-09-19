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

  // Type reflections
  static_assert(std::is_same_v<void_type_info, decltype(reflexpr(void))>);
  
  static_assert(std::is_same_v<character_type_info, decltype(reflexpr(char))>);
  static_assert(std::is_same_v<character_type_info, decltype(reflexpr(char16_t))>);
  static_assert(std::is_same_v<character_type_info, decltype(reflexpr(char32_t))>);
  static_assert(std::is_same_v<character_type_info, decltype(reflexpr(wchar_t))>);

  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(bool))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(short))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(int))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(long))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(long long))>);

  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(unsigned short))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(unsigned int))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(unsigned long))>);
  static_assert(std::is_same_v<integral_type_info, decltype(reflexpr(unsigned long long))>);
  
  static_assert(std::is_same_v<floating_point_type_info, decltype(reflexpr(float))>);
  static_assert(std::is_same_v<floating_point_type_info, decltype(reflexpr(double))>);
  static_assert(std::is_same_v<floating_point_type_info, decltype(reflexpr(long double))>);

  static_assert(std::is_same_v<pointer_type_info, decltype(reflexpr(int*))>);
  static_assert(std::is_same_v<array_type_info, decltype(reflexpr(int[3]))>);
  static_assert(std::is_same_v<array_type_info, decltype(reflexpr(int[]))>);

  static_assert(std::is_same_v<function_type_info, decltype(reflexpr(auto()->void))>);
  static_assert(std::is_same_v<function_type_info, decltype(reflexpr(auto(int, int)->void))>);

  // FIXME: This doesn't parse. It should.
  // static_assert(function_type_info(reflexpr( void() )));

  // FIXME: Write more tests.
  static_assert(reflexpr(void).index() == void_type);
  static_assert(reflexpr(char).index() == character_type);
  static_assert(reflexpr(bool).index() == integral_type);
  static_assert(reflexpr(int).index() == integral_type);
  static_assert(reflexpr(float).index() == floating_point_type);

  // Declaration tests
  static_assert(std::is_same_v<variable_info, decltype(reflexpr(local))>);
  static_assert(std::is_same_v<function_info, decltype(reflexpr(main))>);
  static_assert(std::is_same_v<parameter_info, decltype(reflexpr(argc))>);
  static_assert(std::is_same_v<class_info, decltype(reflexpr(S))>);
  static_assert(std::is_same_v<class_info, decltype(reflexpr(struct S))>);
  static_assert(std::is_same_v<union_info, decltype(reflexpr(U))>);
  static_assert(std::is_same_v<union_info, decltype(reflexpr(union U))>);
  static_assert(std::is_same_v<member_variable_info, decltype(reflexpr(S::m))>);
  static_assert(std::is_same_v<member_function_info, decltype(reflexpr(S::f))>);
  static_assert(std::is_same_v<enum_info, decltype(reflexpr(E))>);
  static_assert(std::is_same_v<enum_info, decltype(reflexpr(enum E))>);
  static_assert(std::is_same_v<enumerator_info, decltype(reflexpr(X))>);
  static_assert(std::is_same_v<namespace_info, decltype(reflexpr(N))>);

  static_assert(reflexpr(local).index() == variable_decl);
  static_assert(reflexpr(global).index() == variable_decl);
  static_assert(reflexpr(main).index() == function_decl);
  static_assert(reflexpr(argc).index() == parameter_decl);
  static_assert(reflexpr(S).index() == class_decl);
  static_assert(reflexpr(struct S).index() == class_decl);
  static_assert(reflexpr(U).index() == union_decl);
  static_assert(reflexpr(union U).index() == union_decl);
  static_assert(reflexpr(S::m).index() == member_variable_decl);
  static_assert(reflexpr(S::f).index() == member_function_decl);
  static_assert(reflexpr(U::m).index() == member_variable_decl);
  static_assert(reflexpr(E).index() == enum_decl);
  static_assert(reflexpr(X).index() == enumerator_decl);
  static_assert(reflexpr(N).index() == namespace_decl);

} 
