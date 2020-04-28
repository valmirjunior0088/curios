module Curios.Term
  (Universe
  ,Index
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

newtype Scope scope =
  Scope scope
  deriving (Eq)
  
data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (Eq)

data Term =
  TePrimitive Primitive |
  TeLiteral Literal |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Unique Term |
  TeType Universe |
  TePiAbstraction Term (Scope Term) |
  TeLambdaAbstraction Term (Scope Term) |
  TeApplication Term Term
  deriving (Eq)

teAbstract :: Name -> Term -> Scope Term
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

teInstantiate :: Term -> Scope Term -> Term
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
