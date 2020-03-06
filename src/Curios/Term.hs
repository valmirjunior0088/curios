module Curios.Term
  (Universe
  ,Index
  ,Type
  ,Scope (..)
  ,Primitive (..)
  ,Literal (..)
  ,Term (..)
  ,teAbstract
  ,teInstantiate
  )
  where

import Curios.Expression
  (Name (..)
  ,QualifiedName (..)
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

type Type =
  Term

newtype Scope scope =
  Scope scope
  
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
  TePiAbstraction Type (Scope Term) |
  TeLambdaAbstraction Type (Scope Term) |
  TeApplication Term Term |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Unique Type |
  TePrimitive Primitive |
  TeLiteral Literal

teAbstract :: Name -> Term -> Scope Term
teAbstract name =
  Scope . go 0 where
    go depth term =
      case term of
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TeFreeVariable (QualifiedName [name']) | name == name' ->
          TeBoundVariable depth
        _ ->
          term

teInstantiate :: Term -> Scope Term -> Term
teInstantiate source (Scope body) =
  go 0 body where
    go depth term =
      case term of
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TeBoundVariable index | index == depth ->
          source
        _ ->
          term
