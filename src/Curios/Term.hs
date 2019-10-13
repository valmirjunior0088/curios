module Curios.Term
  ( Constant (..)
  , Scope (..)
  , Term (..)
  )
  where

import Curios.Expression
  ( Identifier (..)
  , Literal (..)
  )

data Constant =
  CoType Integer |
  CoCharacter |
  CoString |
  CoInteger |
  CoRational
  deriving (Show)

-- Inspired by Edwin Brady's "A practical implementation of a 
-- dependently typed functional programming language"
data Scope a =
  Scope a
  deriving (Show)

data Term =
  TeConstant Constant |
  TePiAbstraction Term (Scope Term) |
  TeLambdaAbstraction Term (Scope Term) |
  TeApplication Term Term |
  TeLiteral Literal |
  TeFreeVariable Identifier |
  TeBoundVariable Integer
  deriving (Show)