module Curios.Term
  ( Universe
  , Index
  , Type
  , Primitive (..)
  , Literal (..)
  , Term (..)
  , teAbstract
  )
  where

import Curios.Expression
  ( Name
  , Identifier
  )

import Data.Unique
  ( Unique
  )

type Universe =
  Integer

type Index =
  Integer

type Type =
  Term
  
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
  TeFreeVariable Identifier |
  TeBoundVariable Index |
  TeMetaVariable Unique Type |
  TePrimitive Primitive |
  TeLiteral Literal

teAbstract :: Name -> Term -> Term
teAbstract name =
  go 0 where
    go depth term =
      case term of
        TePiAbstraction variableType abstractionBody ->
          TePiAbstraction (go depth variableType) (go (depth + 1) abstractionBody)
        TeLambdaAbstraction variableType abstractionBody ->
          TeLambdaAbstraction (go depth variableType) (go (depth + 1) abstractionBody)
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TeFreeVariable [name'] | name == name' ->
          TeBoundVariable depth
        _ ->
          term

