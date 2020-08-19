module Curios.Expression
  (Literal (..)
  ,Identifier
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )
  where

data Literal =
  LtCharacter Char |
  LtText String |
  LtInteger Integer |
  LtRational Double
  deriving (Eq, Show)

type Identifier =
  String

data Binding =
  Binding Identifier Expression
  deriving (Eq, Show)

data Expression =
  ExLiteral Literal |
  ExAbstractionType [Binding] Expression |
  ExAbstraction [Binding] Expression |
  ExApplication Expression [Expression] |
  ExIdentifier Identifier
  deriving (Eq, Show)

data Statement =
  StDef Identifier Expression
  deriving (Eq, Show)
