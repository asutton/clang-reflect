//===--- ReFlection.cpp - Classes for representing reflection ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Reflection class.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Reflection.h"
using namespace clang;


Reflection
Reflection::FromOpaqueValue(std::intptr_t N) {
  using Helper = llvm::detail::PointerSumTypeHelper<
      ReflectedEntityKind, 
      llvm::PointerSumTypeMember<REK_null, void *>,
      llvm::PointerSumTypeMember<REK_decl, NamedDecl *>,
      llvm::PointerSumTypeMember<REK_type, Type *>>;

  ReflectedEntityKind K = (ReflectedEntityKind)(N & Helper::TagMask);
  void *P = (void *)(N & Helper::PointerMask);
  return FromKindAndPtr(K, P);
}

Reflection
Reflection::FromKindAndPtr(ReflectedEntityKind K, void* P) {
  switch (K) {
    case REK_null: return Reflection();
    case REK_decl: return Reflection::ReflectDeclaration((NamedDecl*)P);
    case REK_type: return Reflection::ReflectType((Type*)P);
  }
  llvm_unreachable("Invalid reflection kind");
}
