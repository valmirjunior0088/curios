module Curios.Source.Types
  (Literal (..)
  ,Identifier (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Expression (..)
  ,Variable (..)
  ,Variables (..)
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

data Variable =
  Variable SourcePos Identifier Expression

data Variables =
  Variables SourcePos [Variable]

data Statement =
  StLet SourcePos Identifier Variables Expression Expression

data Program =
  Program SourcePos [Statement]
