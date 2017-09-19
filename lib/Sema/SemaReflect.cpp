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
    Kind = REK_null;
    Entity = nullptr;
    return false;
  }

  // Reflect the named entity.
  Entity = R.getAsSingle<NamedDecl>();
  Kind = REK_decl;
  return true;
}

/// Populates the kind and entity with the encoded reflection of the type.
bool Sema::ActOnReflectedType(Declarator &D, unsigned &Kind, 
                              ParsedReflectionPtr &Entity) {
  TypeSourceInfo *TSI = GetTypeForDeclarator(D, CurScope);
  QualType T = TSI->getType();
  if (TagDecl *TD = T->getAsTagDecl()) {
    // Handle elaborated type specifiers as if they were declarations.
    Entity = TD;
    Kind = REK_decl;
  } else {
    // Otherwise, this is a type reflection.
    Entity = const_cast<Type*>(T.getTypePtr());
    Kind = REK_type;
  }
  return true;
}

static QualType GetMetaType(Sema &SemaRef, const char* Name, 
                            SourceLocation Loc) {
  return LookupMetaDecl(SemaRef, Name, Loc);
}

static QualType GetMetaType(Sema &SemaRef, const NamedDecl *D, 
                            SourceLocation Loc) {
  switch (D->getKind()) {
    case Decl::TranslationUnit:
      return GetMetaType(SemaRef, "translation_info", Loc);
    case Decl::Namespace:
      return GetMetaType(SemaRef, "namespace_info", Loc);
    case Decl::Var:
      return GetMetaType(SemaRef, "variable_info", Loc);
    case Decl::Function:
      return GetMetaType(SemaRef, "function_info", Loc);
    case Decl::ParmVar:
      return GetMetaType(SemaRef, "parameter_info", Loc);
    case Decl::CXXRecord: {
      const CXXRecordDecl *Class = cast<CXXRecordDecl>(D);
      if (Class->isUnion())
        return GetMetaType(SemaRef, "union_info", Loc);
      else
        return GetMetaType(SemaRef, "class_info", Loc);
    }
    case Decl::Field:
      return GetMetaType(SemaRef, "member_variable_info", Loc);
    case Decl::CXXMethod:
      return GetMetaType(SemaRef, "member_function_info", Loc);
    case Decl::CXXConstructor:
      return GetMetaType(SemaRef, "constructor_info", Loc);
    case Decl::CXXDestructor:
      return GetMetaType(SemaRef, "destructor_info", Loc);
    case Decl::Enum:
      return GetMetaType(SemaRef, "enum_info", Loc);
    case Decl::EnumConstant:
      return GetMetaType(SemaRef, "enumerator_info", Loc);
    default:
      break;
  }
#ifndef NDEBUG
  D->dump();
#endif
  llvm_unreachable("invalid declaration reflection");
}

static QualType GetMetaType(Sema &SemaRef, const Type *T,
                            SourceLocation Loc) {
  if (T->isVoidType())
    return GetMetaType(SemaRef, "void_type_info", Loc);
  if (T->isAnyCharacterType())
    return GetMetaType(SemaRef, "character_type_info", Loc);
  if (T->isIntegralType(SemaRef.Context))
    return GetMetaType(SemaRef, "integral_type_info", Loc);
  if (T->isFloatingType())
    return GetMetaType(SemaRef, "floating_point_type_info", Loc);
  if (T->isReferenceType())
    return GetMetaType(SemaRef, "reference_type_info", Loc);
  if (T->isFunctionType())
    return GetMetaType(SemaRef, "function_type_info", Loc);
  if (T->isPointerType())
    return GetMetaType(SemaRef, "pointer_type_info", Loc);
  if (T->isArrayType())
    return GetMetaType(SemaRef, "array_type_info", Loc);
#ifndef NDEBUG
  T->dump();
#endif
  llvm_unreachable("invalid type reflection");
}

/// Returns a constant expression that encodes the value of the reflection.
/// The type of expression is determined by the kind of entity reflected.
ExprResult Sema::ActOnCXXReflectExpression(SourceLocation KWLoc, unsigned Kind, 
                                           ParsedReflectionPtr Entity,
                                           SourceLocation LPLoc, 
                                           SourceLocation RPLoc) {
  ReflectionKind REK = (ReflectionKind)Kind;
  Reflection Ref = Reflection::FromKindAndPtr(REK, Entity);

  // Get a type for the entity and compute the type of the expression.
  QualType ReflectionTy = Context.DependentTy;
  QualType OperandTy;
  if (REK == REK_decl) {
    NamedDecl *ND = (NamedDecl *)Entity;
    if (ValueDecl *VD = dyn_cast<ValueDecl>(ND)) 
      OperandTy = VD->getType();
    else if (TypeDecl *TD = dyn_cast<TypeDecl>(ND))
      OperandTy = Context.getTypeDeclType(TD);
    if (!OperandTy.isNull())
      OperandTy = Context.getCanonicalType(OperandTy);
    ReflectionTy = GetMetaType(*this, ND, KWLoc);
  } else if (REK == REK_type) {
    OperandTy = Context.getCanonicalType(QualType((Type *)Entity, 0));
    ReflectionTy = GetMetaType(*this, OperandTy.getTypePtr(), KWLoc);
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
    return new (Context) CXXReflectExpr(KWLoc, Context.DependentTy, Ref, LPLoc, 
                                        RPLoc, VK_RValue,
                                        /*IsTypeDependent=*/false, 
                                        /*IsValueDependent=*/IsTypeDependent, 
                                      OperandTy->isInstantiationDependentType(), 
                                  OperandTy->containsUnexpandedParameterPack());

  // The declaration is a non-dependent value decl.
  CXXReflectExpr *Reflect
      = new (Context) CXXReflectExpr(KWLoc, ReflectionTy, Ref, LPLoc, RPLoc, 
                                     VK_RValue,
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
static bool CheckReflectionOperand(Sema &SemaRef, Expr *E, QualType Target) {
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

  // Build a set of converted arguments.
  SmallVector<Expr *, 2> Operands(Args.size());
  if (Kind == BRT_ReflectAddress || Kind == BRT_ReflectValue) {
    if (!CheckValueTraitOperands(*this, Args, Operands))
      return ExprError();
  } else {
    ConvertTraitOperands(*this, Args, Operands);
  }
  
  // Check the type of the first operand. ReflectPrint is polymorphic.
  if (Kind != URT_ReflectPrint) {
    if (!CheckReflectionOperand(*this, Operands[0], MetaDataTy))
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
    case BRT_ReflectAddress:
    case BRT_ReflectValue:
      // Returns 0.
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
      // Returns 0.
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
}
