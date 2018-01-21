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
#include "TypeLocBuilder.h"
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

static QualType LookupMetaDecl(Sema &SemaRef, const char* Name, 
                               SourceLocation Loc) {
  NamespaceDecl *Meta = SemaRef.getCppxMetaNamespace(Loc);
  if (!Meta)
    return QualType();

  // Lookup the meta_info class.
  IdentifierInfo *II = &SemaRef.PP.getIdentifierTable().get(Name);
  LookupResult R(SemaRef, II, SourceLocation(), Sema::LookupAnyName);
  SemaRef.LookupQualifiedName(R, Meta);
  TagDecl *TD = R.getAsSingle<TagDecl>();
  if (!TD) {
    SemaRef.Diag(Loc, diag::err_need_header_before_reflexpr);
    return QualType();
  }
  return SemaRef.Context.getTagDeclType(TD);
}

/// Lookup the declaration named by SS and Id. Populates the the kind
/// and entity with the encoded reflection of the named entity.
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
    // declarations and templates.
    if (R.isAmbiguous())
      Diag(IdLoc, diag::err_reflect_ambiguous_id) << Id;
    else if (R.isOverloadedResult())
      Diag(IdLoc, diag::err_reflect_overloaded_id) << Id;
    else
      Diag(IdLoc, diag::err_reflect_undeclared_id) << Id;
    Kind = REK_special;
    Entity = nullptr;
    return false;
  }

  // Reflect the named entity.
  Entity = R.getAsSingle<NamedDecl>();
  Kind = REK_declaration;
  return true;
}

/// Populates the kind and entity with the encoded reflection of the type.
/// Reflections of user-defined types are handled as entities.
bool Sema::ActOnReflectedType(Declarator &D, unsigned &Kind, 
                              ParsedReflectionPtr &Entity) {
  TypeSourceInfo *TSI = GetTypeForDeclarator(D, CurScope);
  QualType T = TSI->getType();
  // FIXME: There are other types that can be declarations.
  if (TagDecl *TD = T->getAsTagDecl()) {
    // Handle elaborated type specifiers as if they were declarations.
    Entity = TD;
    Kind = REK_declaration;
  } else {
    // Otherwise, this is a type reflection.
    Entity = const_cast<Type*>(T.getTypePtr());
    Kind = REK_type;
  }
  return true;
}

/// Returns a constant expression that encodes the value of the reflection.
/// The type of the reflection is meta::reflection, an enum class.
ExprResult Sema::ActOnCXXReflectExpression(SourceLocation KWLoc, 
                                           unsigned Kind, 
                                           ParsedReflectionPtr Entity,
                                           SourceLocation LPLoc, 
                                           SourceLocation RPLoc) {
  ReflectionKind REK = (ReflectionKind)Kind;
  Reflection R = Reflection::FromKindAndPtr(REK, Entity);

  bool IsValueDependent = false;
  if (const Type* T = R.getAsType()) {
    // A type reflection is dependent if reflected T is dependent.
    IsValueDependent = T->isDependentType();
  } else if (const Decl *D = R.getAsDeclaration()) {
    // A declaration reflection is value dependent if an id-expression 
    // referring to that declaration is type or value dependent. Reflections
    // of non-value declarations (e.g., namespaces) are never dependent.
    if (const ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
      // Build a fake id-expression in order to determine dependence. 
      Expr *E = new (Context) DeclRefExpr(const_cast<ValueDecl*>(VD), false, 
                                          VD->getType(), VK_RValue, KWLoc);
      IsValueDependent = E->isTypeDependent() || E->isValueDependent();
    } 
  }

  // If the reflected declaration is in some way dependent, the reflection
  // is type dependent.
  if (IsValueDependent)
    return new (Context) CXXReflectExpr(KWLoc, Context.DependentTy, R, 
                                        LPLoc, RPLoc, VK_RValue, 
                                        false, 
                                        IsValueDependent,
                                        IsValueDependent,
                                        /*containsUnexpandedPacks=*/false);

  // The type of reflexpr is meta::object.
  QualType Ty = LookupMetaDecl(*this, "object", KWLoc);
  if (Ty.isNull())
    return ExprError();

  return new (Context) CXXReflectExpr(KWLoc, Ty, R, LPLoc, RPLoc, VK_RValue, 
                                      false, IsValueDependent, IsValueDependent, 
                                      false);
}

// Convert each operand to an rvalue.
static void ConvertTraitOperands(Sema &SemaRef, ArrayRef<Expr *> Args, 
                               SmallVectorImpl<Expr *> &Operands) {
  for (std::size_t I = 0; I < Args.size(); ++I) {
    if (Args[I]->isGLValue())
      Operands[I] = ImplicitCastExpr::Create(SemaRef.Context, 
                                             Args[I]->getType(), 
                                             CK_LValueToRValue, Args[I], 
                                             nullptr, VK_RValue);
    else
      Operands[I] = Args[I];
  }
}

// Given two arguments, ensure the first is an rvalue and the second is
// an lvalue.
static bool CheckValueTraitOperands(Sema &SemaRef, ArrayRef<Expr *> Args,
                                    SmallVectorImpl<Expr *> &Operands) {
  assert(Args.size() == 2 && "Expected two operands");

  if (Args[0]->isGLValue())
    Operands[0] = ImplicitCastExpr::Create(SemaRef.Context, 
                                           Args[0]->getType(), 
                                           CK_LValueToRValue, Args[0], 
                                           nullptr, VK_RValue);
  if (!Args[1]->isGLValue()) {
    SemaRef.Diag(Args[1]->getExprLoc(), 
        diag::err_typecheck_expression_not_modifiable_lvalue);
    return false;
  }
  Operands[1] = Args[1];
  return true;
}

// Check that the argument has the right type. Ignore references and
// cv-qualifiers on the expression.
static bool CheckReflectionOperand(Sema &SemaRef, Expr *E) {
  // Get the type of the expression.
  QualType Source = E->getType();
  if (Source->isReferenceType()) 
    Source = Source->getPointeeType();  
  Source = Source.getUnqualifiedType();
  Source = SemaRef.Context.getCanonicalType(Source);

  QualType Expected = LookupMetaDecl(SemaRef, "object", E->getExprLoc());

  // FIXME: We should cache meta::object and simply compare against that.
  if (Source != Expected) {
    SemaRef.Diag(E->getLocStart(), diag::err_reflection_trait_wrong_type) 
        << Source;
    return false;
  }

  return true;
}

ExprResult Sema::ActOnCXXReflectionTrait(SourceLocation TraitLoc,
                                         ReflectionTrait Trait,
                                         ArrayRef<Expr *> Args,
                                         SourceLocation RParenLoc) {
  QualType ObjectTy = LookupMetaDecl(*this, "object", TraitLoc);
  if (ObjectTy.isNull())
    return ExprError();
  QualType KindTy = LookupMetaDecl(*this, "object_kind", TraitLoc);
  if (KindTy.isNull())
    return ExprError();

  // If any arguments are dependent, then the expression is dependent.
  for (std::size_t I = 0; I < Args.size(); ++I) {
    Expr *Arg = Args[0];
    if (Arg->isTypeDependent() || Arg->isValueDependent())
      return new (Context) CXXReflectionTraitExpr(Context, Context.DependentTy, 
                                                  Trait, TraitLoc, Args, 
                                                  RParenLoc);
  }

  // Build a set of converted arguments.
  SmallVector<Expr *, 2> Operands(Args.size());
  if (Trait == BRT_ReflectAddress || Trait == BRT_ReflectValue) {
    if (!CheckValueTraitOperands(*this, Args, Operands))
      return ExprError();
  } else {
    ConvertTraitOperands(*this, Args, Operands);
  }
  
  // Check the type of the first operand. Note: ReflectPrint is polymorphic.
  if (Trait != URT_ReflectPrint) {
    if (!CheckReflectionOperand(*this, Operands[0]))
      return false;
  }

  // FIXME: If the trait allows multiple arguments, check those.
  QualType ResultTy;
  switch (Trait) {
    case URT_ReflectIndex: // meta::object_kind
      ResultTy = KindTy;
      break;
    
    case URT_ReflectContext: 
    case URT_ReflectHome: 
    case URT_ReflectBegin: 
    case URT_ReflectEnd: 
    case URT_ReflectNext: 
    case URT_ReflectType: // meta::object
      ResultTy = ObjectTy;
      break;
    
    case URT_ReflectName: // const char*
      ResultTy = Context.getPointerType(Context.CharTy.withConst());
      break;

    case BRT_ReflectAddress:
    case BRT_ReflectValue: // int (always 0)
      // These accept an address as a 2nd argument and assign to that
      // that value.
      ResultTy = Context.IntTy;
      break;

    case URT_ReflectTraits: // unsigned
      ResultTy = Context.UnsignedIntTy;
      break;

    case URT_ReflectPrint: // int (always 0)
      // Returns 0.
      ResultTy = Context.IntTy;
      break;
  }
  assert(!ResultTy.isNull() && "unknown reflection trait");

  return new (Context) CXXReflectionTraitExpr(Context, ResultTy, Trait, 
                                              TraitLoc, Operands, RParenLoc);
}

// Check that T is one of the meta:: types. Returns false if not.
static bool CheckReflectionType(Sema &SemaRef, QualType T, SourceLocation Loc) {
  // The operand must be a reflection.
  if (!T->isRecordType()) {
    SemaRef.Diag(Loc, diag::err_expression_not_reflection);
    return false;
  }
  CXXRecordDecl *Class = T->getAsCXXRecordDecl();

  // The expression must be one of the meta::*_info classes. Detect this
  // by searching for a 'T::construct', which is not canonical, but sufficient
  // for now.
  DeclarationNameInfo DNI(&SemaRef.Context.Idents.get("construct"), Loc);
  LookupResult R(SemaRef, DNI, Sema::LookupMemberName);
  SemaRef.LookupQualifiedName(R, Class);
  if (!R.isSingleResult() || !R.getAsSingle<VarDecl>()) {
    SemaRef.Diag(Loc, diag::err_expression_not_reflection);
    return false;
  }

  return true;
}

ExprResult Sema::ActOnCXXReflectedValueExpression(SourceLocation Loc, 
                                                  Expr *Reflection) {
  return BuildCXXReflectedValueExpression(Loc, Reflection);
}

ExprResult Sema::BuildCXXReflectedValueExpression(SourceLocation Loc, 
                                                  Expr *E) {
  // Don't act on dependent expressions, just preserve them.
  if (E->isTypeDependent() || E->isValueDependent())
    return new (Context) CXXReflectedValueExpr(E, Context.DependentTy,
                                               VK_RValue, Loc);

  // The operand must be a reflection.
  if (E->getType() != Context.getUIntPtrType() &&
      !CheckReflectionType(*this, E->getType(), E->getExprLoc()))
    return ExprError();

  // Evaluate the reflection.
  SmallVector<PartialDiagnosticAt, 4> Diags;
  Expr::EvalResult Result;
  Result.Diag = &Diags;
  if (!E->EvaluateAsRValue(Result, Context)) {
    Diag(E->getExprLoc(), diag::reflection_not_constant_expression);
    for (PartialDiagnosticAt PD : Diags)
      Diag(PD.first, PD.second);
    return ExprError();
  }

  // Unpack the reflection.
  std::uintptr_t Handle = 0;
  if (E->getType() == Context.getUIntPtrType()) {
    Handle = Result.Val.getInt().getExtValue();
  } else {
    const APValue &MetaData = Result.Val.getStructField(0);
    const APValue &Field = MetaData.getStructField(1);
    Handle = Field.getInt().getExtValue();
  }
  if (!Handle) {
    Diag(E->getExprLoc(), diag::err_empty_type_reflection);
    return ExprError();
  }
  
  Diag(E->getExprLoc(), diag::err_expression_not_type_reflection);
  return ExprError();
}

/// Evaluates the given expression and yields the computed type.
TypeResult Sema::ActOnReflectedTypeSpecifier(SourceLocation TypenameLoc,
                                             Expr *E) {
  QualType T = BuildReflectedType(TypenameLoc, E);
  if (T.isNull())
    return TypeResult(true);

  // FIXME: Add parens?
  TypeLocBuilder TLB;
  ReflectedTypeLoc TL = TLB.push<ReflectedTypeLoc>(T);
  TL.setNameLoc(TypenameLoc);
  TypeSourceInfo *TSI = TLB.getTypeSourceInfo(Context, T);
  return CreateParsedType(T, TSI);
}

/// Evaluates the given expression and yields the computed type.
QualType Sema::BuildReflectedType(SourceLocation TypenameLoc, Expr *E) {
  if (E->isTypeDependent() || E->isValueDependent())
    return Context.getReflectedType(E, Context.DependentTy);

  // The operand must be a reflection.
  if (!CheckReflectionType(*this, E->getType(), E->getExprLoc()))
    return QualType();

  // Evaluate the reflection.
  SmallVector<PartialDiagnosticAt, 4> Diags;
  Expr::EvalResult Result;
  Result.Diag = &Diags;
  if (!E->EvaluateAsRValue(Result, Context)) {
    Diag(E->getExprLoc(), diag::reflection_not_constant_expression);
    for (PartialDiagnosticAt PD : Diags)
      Diag(PD.first, PD.second);
    return QualType();
  }

  // Decode the reflected value.
#if 0
  Reflection Refl;
  const APValue &MetaData = Result.Val.getStructField(0);
  const APValue &Field = MetaData.getStructField(1);
  std::uintptr_t Handle = Field.getInt().getExtValue();
  if (!Handle) {
    Diag(E->getExprLoc(), diag::err_empty_type_reflection);
    return QualType();
  }
  else if (!Context.GetReflection(Handle, Refl)) {
    Diag(E->getExprLoc(), diag::err_reflection_not_known);
    return QualType();
  }

  QualType Computed;
  if (Decl *D = Refl.getAsDeclaration()) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      Computed = VD->getType();
    if (TypeDecl *TD = dyn_cast<TypeDecl>(D))
      Computed = Context.getTypeDeclType(TD);
  } else if (Type *T = Refl.getAsType()) {
    Computed = QualType(T, 0);
  }

  if (Computed.isNull()) {
    Diag(E->getExprLoc(), diag::err_expression_not_type_reflection);
    return QualType();  
  }
  return Context.getReflectedType(E, Computed);
#endif
  return QualType();
}
