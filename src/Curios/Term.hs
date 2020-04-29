module Curios.Term
  (Primitive (..)
  ,Index
  ,Universe
  ,Scope (..)
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

data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (Show)

type Index =
  Natural

type Universe =
  Natural

newtype Scope =
  Scope Term

data Term =
  TePrimitive Primitive |
  TeLiteral Literal |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Unique Term |
  TeType Universe |
  TePiAbstraction Term Scope |
  TeLambdaAbstraction Term Scope |
  TeApplication Term Term

teAbstract :: Name -> Term -> Scope
teAbstract name =
  Scope . go 0 where
    go depth term =
      case term of
        TeFreeVariable (QualifiedName [] name') | name == name' ->
          TeBoundVariable depth
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        _ ->
          term

teInstantiate :: Term -> Scope -> Term
teInstantiate source (Scope body) =
  go 0 body where
    go depth term =
      case term of
        TeBoundVariable index | index == depth ->
          source
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TePiAbstraction variableType (Scope abstractionBody) ->
          TePiAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeLambdaAbstraction variableType (Scope abstractionBody) ->
          TeLambdaAbstraction (go depth variableType) (Scope (go (depth + 1) abstractionBody))
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        _ ->
          term
