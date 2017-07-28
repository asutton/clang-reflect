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

#include "clang/AST/APValue.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/DenseMapInfo.h"

namespace clang {

/// \brief The kind of construct reflected.
enum ReflectionKind { 
  /// \brief An empty reflection.
  REK_null,
  
  /// \brief A reflection of a named entity, possibly a namespace.
  REK_decl,

  /// \brief A reflection of a type.
  REK_type,

  /// \brief A base class specifier,
  REK_base,
};

/// Information about a reflected construct.
class Reflection {
private:
  ReflectionKind Kind;
  void *Ptr;

  Reflection(ReflectionKind K, void* P)
    : Kind(K), Ptr(P) { }

public:
  /// \brief Creates an empty reflection.
  Reflection() 
    : Kind(REK_null), Ptr() { }

  /// \brief Creates a reflection of a declaration.
  static Reflection ReflectDeclaration(Decl *D) { return {REK_decl, D}; }

  /// \brief Creates a reflection of a type.
  static Reflection ReflectType(Type *T) { return {REK_type, T}; }

  /// \brief Creates a reflection from an entity kind a pointer.
  static Reflection FromKindAndPtr(ReflectionKind K, void* P) { return {K, P}; }

  /// \brief Converts to true when the reflection is valid.
  explicit operator bool() const { return !isNull(); }

  /// \brief The kind of construct reflected.
  ReflectionKind getKind() const { return Kind; }

  /// \brief The opaque pointer.
  const void* getOpaquePointer() const { return Ptr; }

  /// \brief True if the reflection is null.
  bool isNull() const { return Kind == REK_null; }

  /// \brief True if this is a reflected declaration.
  bool isDeclaration() const { return Kind == REK_decl; }
  
  /// \brief Returns true if this is a reflected type.
  bool isType() const { return Kind == REK_type; }

  /// \brief The reflected declaration.
  Decl *getDeclaration() { 
    assert(isDeclaration() && "Not a declaration");
    return (Decl*)Ptr;
  }

  /// \brief The reflected declaration or null if not a declaration.
  Decl* getAsDeclaration() {
    return isDeclaration() ? getDeclaration() : nullptr;
  }

  /// \brief The reflected type.
  Type *getType() { 
    assert(isType() && "Not a type");
    return (Type*)Ptr;
  }

  /// \brief The reflected type or null if not a type.
  Type* getAsType() {
    return isType() ? getType() : nullptr;
  }

  /// Returns the metadata object for the reflection.
  APValue getMetaData(ASTContext &Ctx) const;
};

}  // end namespace clang

namespace llvm {
// Specialize DenseMapInfo for reflections. Reflections are hashed and compared
// on the basis of their pointer value only. The kind is implied by the value
// of that pointer (i.e., it is not possible to have two entries reflections
// with the same address, but different kinds).
template<>
struct DenseMapInfo<clang::Reflection> {
  using Reflection = clang::Reflection;
  using Base = DenseMapInfo<void*>;
  static Reflection getEmptyKey() {
    return Reflection::FromKindAndPtr(clang::REK_null, Base::getEmptyKey());
  }
  static Reflection getTombstoneKey() {
    return Reflection::FromKindAndPtr(clang::REK_null, Base::getTombstoneKey());
  }
  static unsigned getHashValue(const Reflection &Val) {
    return Base::getHashValue(Val.getOpaquePointer());
  }
  static bool isEqual(const Reflection &LHS, const Reflection &RHS) {
    return LHS.getOpaquePointer() == RHS.getOpaquePointer();
  }
};
} // namespace llvm

#endif
