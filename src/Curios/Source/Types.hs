module Curios.Source.Types
  (Literal (..)
  ,Identifier (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
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

data FunctionTypeVariable =
  FunctionTypeVariable SourcePos (Maybe Identifier) Expression

data FunctionVariable =
  FunctionVariable SourcePos Identifier

data Expression =
  ExLiteral SourcePos Literal |
  ExIdentifier SourcePos Identifier |
  ExFunctionType SourcePos (Maybe Identifier) [FunctionTypeVariable] Expression |
  ExFunction SourcePos [FunctionVariable] Expression |
  ExApplication SourcePos Expression [Expression]

data Binding =
  Binding SourcePos Identifier Expression

data Prefix =
  Prefix SourcePos [Binding]

data Statement =
  StLet SourcePos Identifier Prefix Expression Expression

data Program =
  Program SourcePos [Statement]
