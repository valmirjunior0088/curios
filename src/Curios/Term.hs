module Curios.Term
  ( Primitive (..) 
  , Term (..)
  )
  where

import Curios.Common
  ( Identifier (..)
  , Literal (..)
  )

data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (show)

-- A LOT IS MISSING, REALLY
data Term =
  TeFreeVariable Identifier |
  TeBoundVariable Integer |
  TeType Integer |
  TePrimitive Primitive |
  TeLiteral Literal |
  TePiAbstraction {-?-} |
  TeLambdaAbstraction {-?-} |
  TeApplication Term Term
  deriving (Show)
