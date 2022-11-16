module WebAssembly.Syntax.Instructions
  ( BlockType (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  )
  where

import WebAssembly.Syntax.Conventions
  ( TypeIdx
  , FuncIdx
  , TableIdx
  , GlobalIdx
  , LocalIdx
  , LabelIdx
  , SymIdx
  , Vec
  )

import WebAssembly.Syntax.Types (ValType)
import Data.Word (Word32)
import Data.Int (Int32, Int64)

data BlockType =
  BlockEmpty |
  BlockValType ValType |
  BlockTypeIdx TypeIdx
  deriving (Show)

data MemArg =
  MemArg { alignment :: Word32, offset :: Word32 }
  deriving (Show)

data Instr =
  Unreachable |
  Nop |
  Block BlockType [Instr] |
  Loop BlockType [Instr] |
  Br LabelIdx |
  BrIf LabelIdx |
  BrTable (Vec LabelIdx) LabelIdx |
  Return |
  Call FuncIdx SymIdx |
  CallIndirect TypeIdx TableIdx |
  Drop |
  LocalGet LocalIdx |
  LocalSet LocalIdx |
  LocalTee LocalIdx |
  GlobalGet GlobalIdx SymIdx |
  GlobalSet GlobalIdx SymIdx |
  I32Load MemArg |
  I32Store MemArg |
  I32Const Int32 |
  I32Add |
  I32Sub |
  I32Mul |
  I32Eq |
  I64Load MemArg |
  I64Store MemArg |
  I64Const Int64 |
  I64Add |
  I64Sub |
  I64Mul |
  F32Load MemArg |
  F32Store MemArg |
  F32Const Float |
  F32Add |
  F32Sub |
  F32Mul |
  F64Load MemArg |
  F64Store MemArg |
  F64Const Double |
  F64Add |
  F64Sub |
  F64Mul |
  I32FuncRef Int32 SymIdx |
  I32DataRef Int32 SymIdx Int32
  deriving (Show)

newtype Expr =
  Expr [Instr]
  deriving (Show)
