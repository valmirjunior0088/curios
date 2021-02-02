module Curios.Source.Types
  (Identifier (..)
  ,Literal (..)
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

data Identifier =
  Identifier SourcePos String

data Literal =
  LtText SourcePos String |
  LtInteger SourcePos Integer |
  LtReal SourcePos Double

data FunctionTypeVariable =
  FunctionTypeVariable SourcePos (Maybe Identifier) Expression

data FunctionVariable =
  FunctionVariable SourcePos Identifier

data Expression =
  ExIdentifier SourcePos Identifier |
  ExLiteral SourcePos Literal |
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
