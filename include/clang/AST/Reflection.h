//===--- Reflection.h - Classes for representing reflection -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines facilities for representing reflected entities.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_REFLECTION_H
#define LLVM_CLANG_AST_REFLECTION_H

#include "clang/AST/Expr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/PointerSumType.h"

namespace clang {

/// \brief The kind of entity reflected.
///
/// These value is packed into the low-order bits of each reflected pointer.
///
/// \todo Generally support the reflection of types?
enum ReflectedEntityKind { 
  /// \brief An empty reflection.
  REK_null = 0,
  
  /// \brief A reflection of a named entity, possibly a namespace.
  REK_decl = 1,

  /// \brief A reflection of a type.
  REK_type = 2,
};

/// The opaque representation of a node reflection.
using ReflectedEntityPtr =
    llvm::PointerSumType<ReflectedEntityKind,
                         llvm::PointerSumTypeMember<REK_null, void *>,
                         llvm::PointerSumTypeMember<REK_decl, NamedDecl *>,
                         llvm::PointerSumTypeMember<REK_type, Type *>>;

/// An encoded reflection.
class Reflection {
private:

  /// The reflected construct (entity + expression).
  ReflectedEntityPtr Val;

  Reflection(ReflectedEntityPtr V)
    : Val(V) { }

public:
  Reflection() 
    : Val() { }

  /// \brief Creates a reflection of a declaration.
  static Reflection ReflectDeclaration(NamedDecl *D) {
    return ReflectedEntityPtr::create<REK_decl>(D);
  }

  /// \brief Creates a reflection of a type.
  static Reflection ReflectType(Type *T) {
    return ReflectedEntityPtr::create<REK_type>(T);
  }

  /// \brief Creates a reflection from the given opaque value.
  static Reflection FromOpaqueValue(std::intptr_t N);

  /// \brief Creates a reflection from an entity kind a pointer.
  static Reflection FromKindAndPtr(ReflectedEntityKind K, void* P);

  /// \brief The opaque representation of the reflection.
  std::intptr_t ToOpaqueValue() const { return Val.getOpaqueValue(); }

  /// \brief Converts to true when the reflection is valid.
  explicit operator bool() const { return !isInvalid(); }

  /// \brief True if the reflection is invalid.
  bool isInvalid() const { return Val.is<REK_null>(); }

  /// \brief True if this is a reflected declaration.
  bool isDeclaration() const { return Val.is<REK_decl>(); }
  
  /// \brief Returns true if this is a reflected type.
  bool isType() const { return Val.is<REK_type>(); }

  /// \brief The reflected declaration or nullptr if not a declaration.
  NamedDecl *getAsDeclaration() { return Val.get<REK_decl>(); }

  /// \brief The reflected type or nullptr if not a type.
  Type *getAsType() { return Val.get<REK_type>(); }
};

}  // end namespace clang

#endif
