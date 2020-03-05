module Curios.Term
  (Universe
  ,Index
  ,Type (..)
  ,Primitive (..)
  ,Literal (..)
  ,Term (..)
  )
  where

import Curios.Expression
  (QualifiedName (..)
  )

import Numeric.Natural
  (Natural
  )

import Data.Unique
  (Unique
  )

type Universe =
  Natural

type Index =
  Natural

newtype Type =
  Type Term
  
data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double

data Term =
  TeType Universe |
  TePiAbstraction Type Term |
  TeLambdaAbstraction Type Term |
  TeApplication Term Term |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Unique Type |
  TePrimitive Primitive |
  TeLiteral Literal
