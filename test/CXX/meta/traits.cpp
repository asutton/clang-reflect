// RUN: echo

#include <cppx/meta>

using namespace cppx::meta;

namespace N1 { }
inline namespace N2 { }

int var1 = 0;
static int var2 = 0;
extern int var3;
inline int var4 = 0;
constexpr int var5 = 0;

void fn1() { }
void fn2();
void fn3() = delete;
inline void fn4() { }

inline void fn5();
void fn5() { }
void fn5();

constexpr int fn6() { return 0; }

struct S1 {
};

struct S2;

struct S3 {
};
struct S3;

struct S4 {
  virtual ~S4() = default;
};

struct S5 : S4 {
  virtual void f() = 0;
};

struct S6 final : S5 {
  void f() override { }
};

union U1 { };
union U2;

struct S {
  int m1;
  mutable int m2;
  static int m3;
  static constexpr int m4 = 0;

  void f1() { };
  void f2();
  void f3() = delete;
};

enum E1 { X1 };

enum E2 : int;

enum class E3 { X3 };

int main(int argc, const char* argv[]) {

  constexpr namespace_info n1 = reflexpr(N1);
  static_assert(!n1.is_inline());
  constexpr namespace_info n2 = reflexpr(N2);
  static_assert(n2.is_inline());

  constexpr variable_info v1 = reflexpr(var1);
  static_assert(v1.has_external_linkage());
  static_assert(v1.has_static_storage());
  static_assert(!v1.is_static());
  static_assert(!v1.is_extern());
  static_assert(!v1.is_inline());
  static_assert(!v1.is_constexpr());

  constexpr variable_info v2 = reflexpr(var2);
  static_assert(v2.has_internal_linkage());
  static_assert(v2.has_static_storage());
  static_assert(v2.is_static());
  static_assert(!v2.is_extern());
  static_assert(!v2.is_inline());
  static_assert(!v2.is_constexpr());

  constexpr variable_info v3 = reflexpr(var3);
  static_assert(v3.has_external_linkage());
  static_assert(v3.has_static_storage());
  static_assert(!v3.is_static());
  static_assert(v3.is_extern());
  static_assert(!v3.is_inline());
  static_assert(!v3.is_constexpr());

  constexpr variable_info v4 = reflexpr(var4);
  static_assert(v4.has_external_linkage());
  static_assert(v4.has_static_storage());
  static_assert(!v4.is_static());
  static_assert(!v4.is_extern());
  static_assert(v4.is_inline());
  static_assert(!v4.is_constexpr());

  constexpr variable_info v5 = reflexpr(var5);
  static_assert(v5.has_internal_linkage()); // REALLY?
  static_assert(v5.has_static_storage());
  static_assert(!v5.is_static());
  static_assert(!v5.is_extern());
  static_assert(!v5.is_inline()); // FIXME: This seems like a bug.
  static_assert(v5.is_constexpr());

  constexpr function_info f1 = reflexpr(fn1);
  static_assert(f1.has_external_linkage());
  static_assert(f1.is_defined());

  constexpr function_info f2 = reflexpr(fn2);
  static_assert(f2.has_external_linkage());
  static_assert(!f2.is_defined());

  constexpr function_info f3 = reflexpr(fn3);
  static_assert(f3.is_defined());
  static_assert(f3.is_deleted());

  constexpr function_info f4 = reflexpr(fn4);
  static_assert(f4.is_inline());

  constexpr function_info f5 = reflexpr(fn5);
  static_assert(f5.is_defined());
  static_assert(f5.is_inline());

  constexpr function_info f6 = reflexpr(fn6);
  static_assert(f6.is_constexpr());
  static_assert(f6.is_inline());

  constexpr class_info c1 = reflexpr(S1);
  static_assert(c1.access() == no_access);
  static_assert(c1.is_complete());
  static_assert(c1.is_empty());

  constexpr class_info c2 = reflexpr(S2);
  static_assert(!c2.is_complete());

  constexpr class_info c3 = reflexpr(S3);
  static_assert(c3.is_complete());
  static_assert(c3.is_empty());

  constexpr class_info c4 = reflexpr(S4);
  static_assert(c4.is_polymorphic());
  static_assert(!c4.is_abstract());

  constexpr class_info c5 = reflexpr(S5);
  static_assert(c5.is_polymorphic());
  static_assert(c5.is_abstract());

  constexpr class_info c6 = reflexpr(S6);
  static_assert(c6.is_polymorphic());
  static_assert(!c6.is_abstract());
  static_assert(c6.is_final());
  static_assert(!c6.is_empty());

  constexpr union_info u1 = reflexpr(U1);
  static_assert(u1.access() == no_access);
  static_assert(u1.is_complete());

  constexpr union_info u2 = reflexpr(U2);
  static_assert(!u2.is_complete());

  constexpr member_variable_info mv1 = reflexpr(S::m1);
  static_assert(mv1.has_automatic_storage());
  static_assert(mv1.is_public());

  constexpr member_variable_info mv2 = reflexpr(S::m2);
  static_assert(mv2.is_mutable());

  constexpr member_variable_info mv3 = reflexpr(S::m3);
  static_assert(mv3.has_static_storage());
  static_assert(mv3.is_static());

  constexpr member_variable_info mv4 = reflexpr(S::m4);
  static_assert(mv4.has_static_storage());
  static_assert(mv4.is_static());
  static_assert(mv4.is_constexpr());
  static_assert(mv4.is_inline());

  constexpr member_function_info mf1 = reflexpr(S::f1);
  static_assert(mf1.is_public());
  static_assert(!mf1.is_static());
  static_assert(mf1.is_defined());
  static_assert(mf1.is_inline());

  constexpr member_function_info mf2 = reflexpr(S::f2);
  static_assert(!mf2.is_defined());

  constexpr member_function_info mf3 = reflexpr(S::f3);
  static_assert(mf3.is_defined());
  static_assert(mf3.is_deleted());

  constexpr enum_info e1 = reflexpr(E1);
  static_assert(e1.access() == no_access);
  static_assert(e1.is_complete());
  static_assert(!e1.is_scoped());

  constexpr enum_info e2 = reflexpr(E2);
  static_assert(e2.is_complete()); // Has fixed underlying type.
  static_assert(!e2.is_scoped());

  constexpr enum_info e3 = reflexpr(E3);
  static_assert(e3.is_complete());
  static_assert(e3.is_scoped());

  constexpr enumerator_info x1 = reflexpr(X1);
  static_assert(e1.access() == no_access);
  
  constexpr enumerator_info x2 = reflexpr(X1);
  static_assert(e1.access() == no_access);
}
