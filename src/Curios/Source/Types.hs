module Curios.Source.Types
  (Literal (..)
  ,Identifier (..)
  ,FunctionTypeBinding (..)
  ,FunctionBinding (..)
  ,Expression (..)
  ,Binding (..)
  ,Statement (..)
  ,Program (..)
  )
  where

import Text.Megaparsec (SourcePos)

data Literal =
  LtText SourcePos String |
  LtInteger SourcePos Int |
  LtReal SourcePos Double

data Identifier =
  Identifier SourcePos String

data FunctionTypeBinding =
  FunctionTypeBinding SourcePos (Maybe Identifier) (Maybe Identifier) Expression

data FunctionBinding =
  FunctionBinding SourcePos Identifier

data Expression =
  ExLiteral SourcePos Literal |
  ExIdentifier SourcePos Identifier |
  ExFunctionType SourcePos [FunctionTypeBinding] Expression |
  ExFunction SourcePos [FunctionBinding] Expression |
  ExApplication SourcePos Expression [Expression] |
  ExParens SourcePos Expression

data Binding =
  Binding SourcePos Identifier Expression

data Statement =
  StDefn SourcePos Identifier [Binding] Expression Expression

data Program =
  Program SourcePos [Statement]
