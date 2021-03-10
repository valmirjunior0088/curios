module Curios.Source.Types
  (Literal (..)
  ,Identifier (..)
  ,FunctionTypeBinding (..)
  ,FunctionBinding (..)
  ,Expression (..)
  ,Binding (..)
  ,Prefix (..)
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
  FunctionTypeBinding SourcePos (Maybe Identifier) Expression

data FunctionBinding =
  FunctionBinding SourcePos Identifier

data Expression =
  ExLiteral SourcePos Literal |
  ExIdentifier SourcePos Identifier |
  ExFunctionType SourcePos (Maybe Identifier) [FunctionTypeBinding] Expression |
  ExFunction SourcePos [FunctionBinding] Expression |
  ExApplication SourcePos Expression [Expression]

data Binding =
  Binding SourcePos Identifier Expression

data Prefix =
  Prefix SourcePos [Binding]

data Statement =
  Statement SourcePos Identifier Prefix Expression Expression

data Program =
  Program SourcePos [Statement]
