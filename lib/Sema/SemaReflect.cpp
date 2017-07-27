//===--- SemaReflect.cpp - Semantic Analysis for Reflection ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for reflection.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/SemaInternal.h"
using namespace clang;
using namespace sema;

/// \brief Returns the cppx namespace if a suitable header has been included.
/// If not, a diagnostic is emitted, and nullptr is returned.
NamespaceDecl *Sema::getCppxNamespace(SourceLocation Loc) {
  if (CppxNamespace)
    return CppxNamespace;

  IdentifierInfo *CppxII = &PP.getIdentifierTable().get("cppx");
  LookupResult R(*this, CppxII, Loc, LookupNamespaceName);
  LookupQualifiedName(R, Context.getTranslationUnitDecl());
  if (!R.isSingleResult()) {
    Diag(Loc, diag::err_need_header_before_reflexpr);
    return nullptr;
  }
  NamespaceDecl *Ns = R.getAsSingle<NamespaceDecl>();
  assert(Ns && "cppx is not a namespace");
  CppxNamespace = Ns;
  return CppxNamespace;
}

/// \brief Same as RequireCppxNamespace, but requires cppx::meta.
NamespaceDecl *Sema::getCppxMetaNamespace(SourceLocation Loc) {
  if (CppxMetaNamespace)
    return CppxMetaNamespace;

  NamespaceDecl *Cppx = getCppxNamespace(Loc);
  if (!Cppx)
    return nullptr;

  // Get the cppx::meta namespace.
  IdentifierInfo *MetaII = &PP.getIdentifierTable().get("meta");
  LookupResult R(*this, MetaII, Loc, LookupNamespaceName);
  LookupQualifiedName(R, Cppx);
  if (!R.isSingleResult()) {
    Diag(Loc, diag::err_need_header_before_reflexpr);
    return nullptr;
  }
  NamespaceDecl *Ns = R.getAsSingle<NamespaceDecl>();
  assert(Ns && "cppx::meta is not a namespace");
  CppxMetaNamespace = Ns;
  return CppxMetaNamespace;
}

/// \brief Returns the class type cpp::meta::meta_info.
QualType Sema::getMetaInfoType(SourceLocation Loc) {
  if (MetaInfoDecl)
    return Context.getRecordType(MetaInfoDecl);

  NamespaceDecl *Meta = getCppxMetaNamespace(Loc);
  if (!Meta)
    return QualType();

  // Lookup the met_info class.
  IdentifierInfo *II = &PP.getIdentifierTable().get("meta_info");
  LookupResult R(*this, II, SourceLocation(), LookupAnyName);
  LookupQualifiedName(R, Meta);
  CXXRecordDecl *Decl = R.getAsSingle<CXXRecordDecl>();
  if (!Decl) {
    Diag(Loc, diag::err_need_header_before_reflexpr);
    return QualType();
  }
  MetaInfoDecl = Decl;
  return Context.getRecordType(MetaInfoDecl);
}

/// Lookup the declaration named by SS and Id. Return
bool Sema::ActOnReflectedId(CXXScopeSpec &SS, SourceLocation IdLoc,
                            IdentifierInfo *Id, unsigned &Kind, 
                            ParsedReflectionPtr &Entity) {
  
  // Perform any declaration having the given name.
  LookupResult R(*this, Id, IdLoc, LookupAnyName);
  LookupParsedName(R, CurScope, &SS);
  if (!R.isSingleResult()) {
    // FIXME: Incorporate the scope specifier in the diagnostics. Also note
    // alternatives for an ambiguous lookup.
    //
    // FIXME: I believe that we would eventually like to support overloaded
    // declarations.
    if (R.isAmbiguous())
      Diag(IdLoc, diag::err_reflect_ambiguous_id) << Id;
    else if (R.isOverloadedResult())
      Diag(IdLoc, diag::err_reflect_overloaded_id) << Id;
    else
      Diag(IdLoc, diag::err_reflect_undeclared_id) << Id;
    Kind = REK_null;
    Entity = nullptr;
    return false;
  }

  Kind = REK_decl;
  Entity = R.getAsSingle<NamedDecl>();
  return true;
}

bool Sema::ActOnReflectedType(Declarator &D, unsigned &Kind, 
                              ParsedReflectionPtr &Entity) {
  TypeSourceInfo *TSI = GetTypeForDeclarator(D, CurScope);
  Kind = REK_type;
  Entity = const_cast<Type*>(TSI->getType().getTypePtr());
  return true;
}

ExprResult Sema::ActOnCXXReflectExpression(SourceLocation KWLoc, unsigned Kind, 
                                           ParsedReflectionPtr Entity,
                                           SourceLocation LPLoc, 
                                           SourceLocation RPLoc) {
  
  // Require the right header file whenever the operator is used.
  QualType Meta = getMetaInfoType(KWLoc);
  if (Meta.isNull())
    return ExprError();

  ReflectedEntityKind REK = (ReflectedEntityKind)Kind;
  Reflection Ref = Reflection::FromKindAndPtr(REK, Entity);

  // Get a type for the reflection.
  QualType OperandTy;
  if (REK == REK_decl) {
    NamedDecl *ND = (NamedDecl *)Entity;
    if (ValueDecl *VD = dyn_cast<ValueDecl>(ND))
      OperandTy = VD->getType();
  } else if (REK == REK_type) {
    OperandTy = QualType((Type *)Entity, 0);
  }
  else {
    llvm_unreachable("Invalid reflection");
  }

  bool IsTypeDependent 
    = OperandTy.isNull() ? false : OperandTy->isDependentType();

  // FIXME: There is also the potential for "unresolved" reflections like
  // overload sets. Those are not type dependent, but would have overload type.

  CXXReflectExpr *Reflect;
  if (IsTypeDependent)
    // If the operand is type dependent, the result is value dependent.
    Reflect = new (Context) CXXReflectExpr(KWLoc, Meta, Ref, LPLoc, RPLoc,
                                           VK_RValue, 
                                           /*IsTypeDependent=*/false, 
                                           /*IsValueDependent=*/IsTypeDependent, 
                                      OperandTy->isInstantiationDependentType(), 
                                  OperandTy->containsUnexpandedParameterPack());
  else
    // The declaration is a non-dependent value decl.
    Reflect = new (Context) CXXReflectExpr(KWLoc, Meta, Ref, LPLoc, RPLoc,
                                           VK_RValue, 
                                           /*IsTypeDependent=*/false, 
                                           /*IsValueDependent=*/false, 
                                           /*IsInstantiationDependent=*/false, 
                                           /*ContainsUnexpandedPacks=*/false);

  // Build an expression that constructs a meta_info object.
  QualType IntPtrTy = Context.getIntPtrType();
  llvm::APSInt Value = Context.MakeIntValue(Ref.ToOpaqueValue(), IntPtrTy);
  Expr *Lit = new (Context) IntegerLiteral(Context, Value, IntPtrTy, KWLoc);
  MultiExprArg Args(Lit);
  TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(Meta);
  ExprResult Construct = BuildCXXTypeConstructExpr(TSI, KWLoc, Args, KWLoc);
  Reflect->setConstruction(Construct.get());

  return Reflect;
}

ExprResult Sema::ActOnCXXReflectionTrait(SourceLocation TraitLoc,
                                         ReflectionTrait Kind,
                                         ArrayRef<Expr *> Args,
                                         SourceLocation RParenLoc) {

  // FIXME: Actually check the types of operands.

  QualType ResultTy;
  switch (Kind) {
    case URT_ReflectIndex:
      ResultTy = Context.IntTy;
      break;
  }

  return new (Context) CXXReflectionTraitExpr(Context, ResultTy, Kind, TraitLoc,
                                              Args, RParenLoc);
}
