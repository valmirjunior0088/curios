module WebAssembly.Syntax.Types
  ( NumType (..)
  , VecType (..)
  , RefType (..)
  , ValType (..)
  , i32
  , i64
  , f32
  , f64
  , v128
  , funcRef
  , externRef
  , ResultType (..)
  , FuncType (..)
  , Limits (..)
  , MemType (..)
  , TableType (..)
  , Mut (..)
  , GlobalType (..)
  )
  where

import WebAssembly.Syntax.Conventions (Vec)
import Data.Word (Word32)

data NumType =
  I32 |
  I64 |
  F32 |
  F64
  deriving (Show, Eq)

data VecType =
  V128
  deriving (Show, Eq)

data RefType =
  FuncRef |
  ExternRef
  deriving (Show, Eq)

data ValType =
  ValNumType NumType |
  ValVecType VecType |
  ValRefType RefType
  deriving (Show, Eq)

i32 :: ValType
i32 = ValNumType I32

i64 :: ValType
i64 = ValNumType I64

f32 :: ValType
f32 = ValNumType F32

f64 :: ValType
f64 = ValNumType F64

v128 :: ValType
v128 = ValVecType V128

funcRef :: ValType
funcRef = ValRefType FuncRef

externRef :: ValType
externRef = ValRefType ExternRef

newtype ResultType =
  ResultType (Vec ValType)
  deriving (Show, Eq)

data FuncType =
  FuncType ResultType ResultType
  deriving (Show, Eq)

data Limits =
  Unbounded Word32 |
  Bounded Word32 Word32
  deriving (Show, Eq)

newtype MemType =
  MemType Limits
  deriving (Show, Eq)

data TableType =
  TableType RefType Limits
  deriving (Show, Eq)

data Mut =
  Const |
  Var
  deriving (Show, Eq)

data GlobalType =
  GlobalType ValType Mut
  deriving (Show, Eq)
