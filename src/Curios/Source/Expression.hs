module Curios.Source.Expression
  ( Identifier (..)
  , FunctionTypeBinding (..)
  , FunctionBinding (..)
  , SelfBinding (..)
  , Binding (..)
  , Primitive (..)
  , Literal (..)
  , Operation (..)
  , Expression (..)
  , Item (..)
  , Items (..)
  )
  where

import Text.Megaparsec (SourcePos)
import Data.Int (Int32)

data Identifier =
  Identifier SourcePos String
  deriving (Show)

data FunctionTypeBinding =
  FtDependent SourcePos Identifier Expression |
  FtNonDependent SourcePos Expression
  deriving (Show)

data FunctionBinding =
  FunctionBinding SourcePos Identifier
  deriving (Show)

data SelfBinding =
  SelfBinding SourcePos Identifier
  deriving (Show)

data Binding =
  Binding SourcePos Identifier Expression
  deriving (Show)

data Primitive =
  PrInt32 SourcePos |
  PrFlt32 SourcePos
  deriving (Show)

data Literal =
  LtInt32 SourcePos Int32 |
  LtFlt32 SourcePos Float
  deriving (Show)

data Operation =
  OpInt32Sum SourcePos Expression Expression |
  OpFlt32Sum SourcePos Expression Expression
  deriving (Show)

data Expression =
  ExParentheses SourcePos Expression |
  ExType SourcePos |
  ExFunctionType SourcePos FunctionTypeBinding Expression |
  ExFunction SourcePos FunctionBinding Expression |
  ExApplication SourcePos Expression [Expression] |
  ExIdentifier SourcePos Identifier |
  ExSelf SourcePos SelfBinding Expression |
  ExData SourcePos Expression |
  ExCase SourcePos Expression |
  ExPrimitive SourcePos Primitive |
  ExLiteral SourcePos Literal |
  ExOperation SourcePos Operation
  deriving (Show)

data Item =
  Item SourcePos Identifier [Binding] Expression Expression
  deriving (Show)

data Items =
  Items SourcePos [Item]
  deriving (Show)
