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

type Type =
  Term

data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (Show)

data Term =
  TeFreeVariable Identifier |
  TeBoundVariable Integer |
  TeType Integer |
  TePiAbstraction Type Term |
  TeLambdaAbstraction Type Term |
  TeApplication Term Term |
  TePrimitive Primitive |
  TeLiteral Literal
  deriving (Show)