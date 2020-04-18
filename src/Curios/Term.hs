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
  (Literal (..)
  ,Name (..)
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

data Term =
  TePrimitive Primitive |
  TeLiteral Literal |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Unique Type |
  TeType Universe |
  TePiAbstraction Type (Scope Term) |
  TeLambdaAbstraction Type (Scope Term) |
  TeApplication Term Term

teAbstract :: Name -> Term -> Scope Term
teAbstract name =
  Scope . go 0 where
    go depth term =
      case term of
        TeFreeVariable (QualifiedName [] name') | name == name' ->
          TeBoundVariable depth
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        _ ->
          term

teInstantiate :: Term -> Scope Term -> Term
teInstantiate source (Scope body) =
  go 0 body where
    go depth term =
      case term of
        TeBoundVariable index | index == depth ->
          source
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        _ ->
          term
