{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Curios.Compilation.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , DataIdx (..)
  , SecIdx (..)
  , SymIdx (..)

  , NumberType (..)
  , RefType (..)
  , ValueType (..)
  , i32
  , i64
  , f32
  , f64
  , funcRef
  , externRef
  , ResultType (..)
  , FuncType (..)
  , TypeSec (..)
  , emptyTypeSec

  , MinSize
  , MaxSize
  , Limits (..)
  , TableType (..)
  , MemType (..)
  , Mutability (..)
  , GlobalType (..)
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)
  , emptyImportSec

  , Func (..)
  , FuncSec (..)
  , emptyFuncSec

  , ExportDesc (..)
  , Export (..)
  , ExportSec (..)
  , emptyExportSec

  , Elem (..)
  , ElemSec (..)
  , emptyElemSec

  , Locals (..)
  , Offset
  , Alignment
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)
  , emptyCodeSec

  , SymKind (..)
  , SymFlag (..)
  , SymInfo (..)
  , SymTable (..)
  , LinkSec (..)
  , emptyLinkSec

  , Module (..)
  , emptyModule

  , RelocType (..)
  , RelocEntry (..)
  , RelocSec (..)
  )
  where

import Data.Int (Int32, Int64)
import Data.Word (Word32)

newtype TypeIdx =
  TypeIdx Word32
  deriving (Num)

newtype FuncIdx =
  FuncIdx Word32
  deriving (Num, Enum, Eq)

newtype LocalIdx =
  LocalIdx Word32
  deriving (Num)

newtype TableIdx =
  TableIdx Word32
  deriving (Num)

newtype MemIdx =
  MemIdx Word32
  deriving (Num)

newtype GlobalIdx =
  GlobalIdx Word32
  deriving (Num)

newtype DataIdx =
  DataIdx Word32
  deriving (Num)

newtype SecIdx =
  SecIdx Word32
  deriving (Num, Enum)

newtype SymIdx =
  SymIdx Word32
  deriving (Num)

data NumberType =
  NtI32 |
  NtI64 |
  NtF32 |
  NtF64
  deriving (Eq)

data RefType =
  RfFuncRef |
  RfExternRef
  deriving (Eq)

data ValueType =
  VtNumberType NumberType |
  VtRefType RefType
  deriving (Eq)

i32 :: ValueType
i32 = VtNumberType NtI32

i64 :: ValueType
i64 = VtNumberType NtI64

f32 :: ValueType
f32 = VtNumberType NtF32

f64 :: ValueType
f64 = VtNumberType NtF64

funcRef :: ValueType
funcRef = VtRefType RfFuncRef

externRef :: ValueType
externRef = VtRefType RfExternRef

newtype ResultType =
  ResultType [ValueType]
  deriving (Eq)

data FuncType =
  FuncType ResultType ResultType
  deriving (Eq)

newtype TypeSec =
  TypeSec [FuncType]

emptyTypeSec :: TypeSec
emptyTypeSec = TypeSec []

type MinSize = Word32
type MaxSize = Word32

data Limits =
  LmUnbounded MinSize |
  LmBounded MinSize MaxSize

data TableType =
  TableType RefType Limits

newtype MemType =
  MemType Limits

data Mutability =
  MtConst |
  MtVar

data GlobalType =
  GlobalType ValueType Mutability

data ImportDesc =
  IdFunc TypeIdx |
  IdTable TableType |
  IdMem MemType |
  IdGlobal GlobalType

data Import =
  Import String String ImportDesc

newtype ImportSec =
  ImportSec [Import]

emptyImportSec :: ImportSec
emptyImportSec = ImportSec []

data Func =
  Func TypeIdx

newtype FuncSec =
  FuncSec [Func]

emptyFuncSec :: FuncSec
emptyFuncSec = FuncSec []

data ExportDesc =
  EdFunc FuncIdx |
  EdTable TableIdx |
  EdMem MemIdx |
  EdGlobal GlobalIdx

data Export =
  Export String ExportDesc

newtype ExportSec =
  ExportSec [Export]

emptyExportSec :: ExportSec
emptyExportSec = ExportSec []

data Elem =
  Elem Expr [FuncIdx]

newtype ElemSec =
  ElemSec [Elem]

emptyElemSec :: ElemSec
emptyElemSec = ElemSec []

data Locals =
  Locals Word32 ValueType

type Alignment = Word32
type Offset = Word32

data MemArg =
  MemArg Alignment Offset

data Instr =
  InI32Const Int32 |
  InI32FuncRef Int32 SymIdx |
  InI64Const Int64 |
  InF32Const Float |
  InF64Const Double |
  InI32Load MemArg |
  InLocalGet LocalIdx |
  InLocalSet LocalIdx |
  InLocalTee LocalIdx |
  InCall FuncIdx SymIdx

newtype Expr =
  Expr [Instr]

data Code =
  Code [Locals] Expr

newtype CodeSec =
  CodeSec [Code]

emptyCodeSec :: CodeSec
emptyCodeSec = CodeSec []

data SymKind =
  SkFunction FuncIdx (Maybe String)

data SymFlag =
  SfVisibilityHidden |
  SfUndefined |
  SfExported |
  SfExplicitName
  deriving (Eq)

data SymInfo =
  SymInfo SymKind [SymFlag]

newtype SymTable =
  SymTable [SymInfo]

data LinkSec =
  LinkSec SymTable

emptyLinkSec :: LinkSec
emptyLinkSec = LinkSec (SymTable [])

data RelocType =
  RlFunctionIndexLeb |
  RlTableIndexSleb

data RelocEntry =
  RelocEntry RelocType Offset SymIdx

data RelocSec =
  RelocSec String SecIdx [RelocEntry]

data Module =
  Module
    { mdTypes :: TypeSec
    , mdImports :: ImportSec
    , mdFuncs :: FuncSec
    , mdExports :: ExportSec
    , mdElems :: ElemSec
    , mdCodes :: CodeSec
    , mdLink :: LinkSec
    }

emptyModule :: Module
emptyModule =
  Module
    { mdTypes = emptyTypeSec
    , mdImports = emptyImportSec
    , mdFuncs = emptyFuncSec
    , mdExports = emptyExportSec
    , mdElems = emptyElemSec
    , mdCodes = emptyCodeSec
    , mdLink = emptyLinkSec
    }
