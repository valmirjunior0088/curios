module Curios.Source.Types
  (Literal (..)
  ,Identifier (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  ,pgDeclarations
  ,pgDefinitions
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

data Statement =
  StLet SourcePos Identifier Expression Expression

data Program =
  Program SourcePos [Statement]

pgDeclarations :: Program -> [(Identifier, Expression)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (StLet _ identifier expression _) = (identifier, expression)

pgDefinitions :: Program -> [(Identifier, Expression)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (StLet _ identifier _ expression) = (identifier, expression)
