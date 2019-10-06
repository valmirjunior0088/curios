module Curios.Term
  ( Primitive (..) 
  , Term (..)
  )
  where

import Curios.Expression
  ( Identifier (..)
  , Literal (..)
  )

data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (Show)

-- Inspired by Edwin Brady's "A practical implementation of a 
-- dependently typed functional programming language"
data Scope a =
  Scope a
  deriving (Show)

data Term =
  TeType Integer |
  TePiAbstraction Term (Scope Term) |
  TeLambdaAbstraction Term (Scope Term) |
  TeApplication Term Term |
  TePrimitive Primitive |
  TeLiteral Literal |
  TeFreeVariable Identifier |
  TeBoundVariable Integer
  deriving (Show)