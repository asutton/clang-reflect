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

/// \brief Returns the class type cpp::meta::meta_info.
QualType Sema::getMetaInfoType(SourceLocation Loc) {
  if (MetaInfoDecl)
    return Context.getRecordType(MetaInfoDecl);

  QualType MetaInfoTy = LookupMetaDecl(*this, "meta_info", Loc);
  if (MetaInfoTy.isNull())
    return QualType();
  MetaInfoDecl = MetaInfoTy->getAsCXXRecordDecl();
  return MetaInfoTy;
}

// TODO: Make this a member and cache the result. Just like above.
static QualType getMetaDataType(Sema &SemaRef, SourceLocation Loc) {
  QualType MetaDataTy = LookupMetaDecl(SemaRef, "meta_data", Loc);
  if (MetaDataTy.isNull())
    return QualType();
  assert(MetaDataTy->isRecordType() && "Expected record type");
  return MetaDataTy;
}

// TODO: Make this a member and cache the result. Just like above.
static QualType getConstructKindType(Sema &SemaRef, SourceLocation Loc) {
  QualType KindTy = LookupMetaDecl(SemaRef, "construct_kind", Loc);
  if (KindTy.isNull())
    return QualType();
  assert(KindTy->isEnumeralType() && "Expected enum type");
  return KindTy;
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

  ReflectionKind REK = (ReflectionKind)Kind;
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

  if (IsTypeDependent)
    // If the operand is type dependent, the result is value dependent.
    return new (Context) CXXReflectExpr(KWLoc, Meta, Ref, LPLoc, RPLoc,
                                           VK_RValue, 
                                           /*IsTypeDependent=*/false, 
                                           /*IsValueDependent=*/IsTypeDependent, 
                                      OperandTy->isInstantiationDependentType(), 
                                  OperandTy->containsUnexpandedParameterPack());

  // The declaration is a non-dependent value decl.
  CXXReflectExpr *Reflect
      = new (Context) CXXReflectExpr(KWLoc, Meta, Ref, LPLoc, RPLoc, VK_RValue,
                                     /*IsTypeDependent=*/false, 
                                     /*IsValueDependent=*/false, 
                                     /*IsInstantiationDependent=*/false, 
                                     /*ContainsUnexpandedPacks=*/false);

  // Never defer evaluation.
  SmallVector<PartialDiagnosticAt, 4> Diags;
  Expr::EvalResult Result;
  if (!Reflect->EvaluateAsRValue(Result, Context)) {
    Diag(KWLoc, diag::err_reflection_failed);
    for (PartialDiagnosticAt PD : Diags)
      Diag(PD.first, PD.second);
    return ExprError();
  }

  return new (Context) CXXConstantExpr(Reflect, std::move(Result.Val));
}

// Check that the argument has the right type. Ignore references and
// cv-qualifiers on the expression.
static bool CheckOperandType(Sema &SemaRef, Expr *E, QualType Target) {
  QualType Source = E->getType();
  if (Source->isReferenceType()) 
    Source = Source->getPointeeType();
  Source = Source.getUnqualifiedType();
  if (Source != Target) {
    SemaRef.Diag(E->getLocStart(), diag::err_reflection_trait_wrong_type) 
        << Target;
    return false;
  }
  return true;
}

ExprResult Sema::ActOnCXXReflectionTrait(SourceLocation TraitLoc,
                                         ReflectionTrait Kind,
                                         ArrayRef<Expr *> Args,
                                         SourceLocation RParenLoc) {

  QualType MetaDataTy = getMetaDataType(*this, TraitLoc);
  QualType MetaInfoTy = getMetaInfoType(TraitLoc);

  // Don't do any processing if any arguments are dependent.
  for (std::size_t I = 0; I < Args.size(); ++I) {
    Expr *Arg = Args[0];
    if (Arg->isTypeDependent() || Arg->isValueDependent())
      return new (Context) CXXReflectionTraitExpr(Context, Context.DependentTy, 
                                                  Kind, TraitLoc, Args, 
                                                  RParenLoc);
  }

  // Rewrite the arguments, converting each to an rvalue, if needed.
  SmallVector<Expr *, 2> Operands(Args.size());
  for (std::size_t I = 0; I < Args.size(); ++I) {
    if (Args[I]->isGLValue())
      Operands[I] = ImplicitCastExpr::Create(Context, Args[I]->getType(), 
                                             CK_LValueToRValue, Args[I], 
                                             nullptr, VK_RValue);
    else
      Operands[I] = Args[I];
  }

  // Check the type of the first operand. ReflectPrint is polymorphic.
  if (Kind != URT_ReflectPrint) {
    if (!CheckOperandType(*this, Operands[0], MetaDataTy))
      return false;
  }

  // FIXME: If the trait allows multiple arguments, check those.
  QualType ResultTy;
  switch (Kind) {
    case URT_ReflectIndex:
      ResultTy = getConstructKindType(*this, TraitLoc);
      break;
    case URT_ReflectContext:
      ResultTy = MetaInfoTy;
      break;
    case URT_ReflectName:
      ResultTy = Context.getPointerType(Context.CharTy.withConst());
      break;
    case URT_ReflectType:
      ResultTy = MetaInfoTy;
      break;
    case URT_ReflectValue:
      // FIXME: Wrong!
      ResultTy = Context.IntTy;
      break;
    case URT_ReflectTraits:
      ResultTy = Context.UnsignedIntTy;
      break;
    case URT_ReflectFirstMember:
    case URT_ReflectNextMember:
      // NOTE: The result type is implementation defined. Because decls are
      // linked into lists, we can simply return their reflections and build
      // iterators in the library.
      ResultTy = MetaInfoTy;
      break;
    case URT_ReflectPrint:
      ResultTy = Context.IntTy;
      break;
  }

  if (ResultTy.isNull())
    return ExprError();

  return new (Context) CXXReflectionTraitExpr(Context, ResultTy, Kind, TraitLoc,
                                              Operands, RParenLoc);
}

/// Evaluates the given expression and yields the computed type.
TypeResult Sema::ActOnReflectedTypeSpecifier(SourceLocation TypenameLoc,
                                             Expr *E) {
  QualType T = BuildReflectedType(TypenameLoc, E);
  if (T.isNull())
    return TypeResult(true);

  // FIXME: Add parens?
  // TypeLocBuilder TLB;
  // ReflectedTypeLoc TL = TLB.push<ReflectedTypeLoc>(T);
  // TL.setNameLoc(TypenameLoc);
  // TypeSourceInfo *TSI = TLB.getTypeSourceInfo(Context, T);
  // return CreateParsedType(T, TSI);

  // FIXME: This is not right.
  TypeSourceInfo *TSI = Context.getTrivialTypeSourceInfo(T, TypenameLoc);

  return CreateParsedType(T, TSI);
}

/// Evaluates the given expression and yields the computed type.
QualType Sema::BuildReflectedType(SourceLocation TypenameLoc, Expr *E) {
  if (E->isTypeDependent() || E->isValueDependent())
    return Context.getReflectedType(E, Context.DependentTy);

  // The operand must be a reflection.
  QualType T = E->getType();
  if (!T->isRecordType()) {
    Diag(E->getExprLoc(), diag::err_expression_not_reflection);
    return QualType();
  }
  CXXRecordDecl *Class = T->getAsCXXRecordDecl();

  // The expression must be one of the meta::*_info classes. Detect this
  // by searching for a 'T::construct', which is not canonical, but sufficient
  // for now.
  DeclarationNameInfo DNI(&Context.Idents.get("construct"), TypenameLoc);
  LookupResult R(*this, DNI, Sema::LookupMemberName);
  LookupQualifiedName(R, Class);
  if (!R.isSingleResult() || !R.getAsSingle<VarDecl>()) {
    Diag(E->getExprLoc(), diag::err_expression_not_reflection);
    return QualType();    
  }

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

  if (Decl *D = Refl.getAsDeclaration()) {
    if (TagDecl *TD = dyn_cast<TagDecl>(D))
      return Context.getTagDeclType(TD);
    else if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      return VD->getType();
  } else if (Type *T = Refl.getAsType()) {
    return QualType(T, 0);
  }
  
  Diag(E->getExprLoc(), diag::err_expression_not_type_reflection);
  return QualType();  
}
