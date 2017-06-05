// RUN: %clang_cc1 -fsyntax-only -verify -std=c++1z %s

immediate class C1 {}; // expected-error {{class cannot be marked 'immediate'}}
immediate struct S1 {}; // expected-error {{struct cannot be marked 'immediate'}}
immediate union U1 {}; // expected-error {{union cannot be marked 'immediate'}}
immediate enum E1 {}; // expected-error {{enum cannot be marked 'immediate'}}

template <typename T> immediate class TC1 {}; // expected-error {{class cannot be marked 'immediate'}}
template <typename T> immediate struct TS1 {}; // expected-error {{struct cannot be marked 'immediate'}}
template <typename T> immediate union TU1 {}; // expected-error {{union cannot be marked 'immediate'}}

immediate class C2; // expected-error {{class cannot be marked 'immediate'}}
immediate struct S2; // expected-error {{struct cannot be marked 'immediate'}}
immediate union U2; // expected-error {{union cannot be marked 'immediate'}}

template <typename T> immediate class TC2; // expected-error {{class cannot be marked 'immediate'}}
template <typename T> immediate struct TS2; // expected-error {{struct cannot be marked 'immediate'}}
template <typename T> immediate union TU2; // expected-error {{union cannot be marked 'immediate'}}

class C2 {} immediate; // expected-error {{class cannot be marked 'immediate'}}
struct S2 {} immediate; // expected-error {{struct cannot be marked 'immediate'}}
union U2 {} immediate; // expected-error {{union cannot be marked 'immediate'}}
enum E2 {} immediate; // expected-error {{enum cannot be marked 'immediate'}}

immediate int x = 0; // expected-error {{variable cannot be marked 'immediate'}}

struct S3 {
  immediate ~S3() { } // expected-error {{destructor cannot be marked 'immediate'}}
};

immediate int f1() { return 0; }
immediate constexpr int f2() { return 0; }
constexpr immediate int f3() { return 0; }
immediate immediate int f4() { return 0; } // expected-warning {{duplicate 'immediate' declaration specifier}}

struct S4 {
  static immediate int f1() { return 1; }
};
