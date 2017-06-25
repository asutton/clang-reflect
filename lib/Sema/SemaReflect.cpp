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
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/SemaInternal.h"
using namespace clang;
using namespace sema;


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

namespace {

// Helper class for building reflections.
struct ReflectionBuilder
{
  ASTContext &Ctx;
  Reflection Ref;
  SourceLocation KW, LP, RP;

  ExprResult MakeDependent(QualType T) {
    return new (Ctx) CXXReflectExpr(KW, Ctx.DependentTy, Ref, LP, RP,
                                    VK_RValue,
                                    /*IsTypeDependent=*/true, 
                                    /*IsValueDependent=*/false, 
                                    T->isInstantiationDependentType(),
                                    T->containsUnexpandedParameterPack());
  }
  ExprResult MakeOrdinary() {
    // FIXME: The type is clearly wrong.
    return new (Ctx) CXXReflectExpr(KW, Ctx.IntTy, Ref, LP, RP,
                                    VK_RValue,
                                    /*IsTypeDependent=*/false, 
                                    /*IsValueDependent=*/false, 
                                    /*IsInstantiationDependent=*/false,
                                    /*ContainsUnexpandedPacks=*/false);
  }
};

} // end namespace

ExprResult Sema::ActOnCXXReflectExpression(SourceLocation KWLoc, unsigned Kind, 
                                           ParsedReflectionPtr Entity,
                                           SourceLocation LP, 
                                           SourceLocation RP) {
  
  ReflectedEntityKind REK = (ReflectedEntityKind)Kind;
  Reflection Ref = Reflection::FromKindAndPtr(REK, Entity);
  
  ReflectionBuilder Builder{Context, Ref, KWLoc, LP, RP};

  // Get a type for the reflection.
  QualType Ty;
  if (REK == REK_decl) {
    NamedDecl *ND = (NamedDecl *)Entity;
    if (ValueDecl *VD = dyn_cast<ValueDecl>(ND))
      Ty = VD->getType();
  } else if (REK == REK_type) {
    Ty = QualType((Type *)Entity, 0);
  }
  else {
    llvm_unreachable("Invalid reflection");
  }

  bool IsTypeDependent = Ty.isNull() ? false : Ty->isDependentType();

  // FIXME: There is also the potential for "unresolved" reflections like
  // overload sets. Those are not type dependent, but would have overload type.

  if (Ty.isNull())
    // The declaration is untyped.
    return Builder.MakeOrdinary();
  else if (IsTypeDependent)
    // The declaration or type is dependent.
    return Builder.MakeDependent(Ty);
  else
    // The declaration is a non-dependent value decl.
    return Builder.MakeOrdinary();
}
