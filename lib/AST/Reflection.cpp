//===--- Reflection.cpp - Classes for representing reflection ---*- C++ -*-===//
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
#include "clang/AST/ASTContext.h"
using namespace clang;

// FIXME: The resulting structure must match *exactly* the `meta_data`
// class in cppx::meta. If not, bad things will happen.
APValue Reflection::getMetaData(ASTContext& Ctx) const {
  llvm::APSInt Category = Ctx.MakeIntValue(getKind(), Ctx.UnsignedIntTy);
  llvm::APSInt Handle = Ctx.MakeIntValue(reinterpret_cast<std::uintptr_t>(Ptr), 
                                         Ctx.getUIntPtrType());

  APValue Result(APValue::UninitStruct(), 0, 2);
  Result.getStructField(0) = APValue(Category);
  Result.getStructField(1) = APValue(Handle);
  return Result;
}

