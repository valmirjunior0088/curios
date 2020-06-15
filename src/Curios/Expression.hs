module Curios.Expression
  (Literal (..)
  ,Name (..)
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

newtype Name =
  Name String
  deriving (Eq, Show, Ord)

data Binding =
  Binding Name Expression
  deriving (Eq, Show)

data Expression =
  ExLiteral Literal |
  ExVariable Name |
  ExAbstractionType [Binding] Expression |
  ExAbstraction [Binding] Expression|
  ExApplication Expression [Expression]
  deriving (Eq, Show)

data Statement =
  StDef Name Expression
  deriving (Eq, Show)
