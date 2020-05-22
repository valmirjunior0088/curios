module Curios.Term
  (Primitive (..)
  ,Index (..)
  ,Universe (..)
  ,Scope (..)
  ,Term (..)
  ,teAbstract
  ,teInstantiate
  ,teSubstitute
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  )

import Curios.Identifier
  (Identifier (..)
  )

import Numeric.Natural
  (Natural
  )

data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational
  deriving (Eq, Show)

newtype Index =
  Index Natural
  deriving (Eq, Show)

newtype Universe =
  Universe Natural
  deriving (Eq, Show)

newtype Scope =
  Scope Term
  deriving (Eq, Show)

data Term =
  TePrimitive Primitive |
  TeLiteral Literal |
  TeFreeVariable QualifiedName |
  TeBoundVariable Index |
  TeMetaVariable Identifier Term |
  TeType Universe |
  TePiAbstraction Term Scope |
  TeLambdaAbstraction Term Scope |
  TeApplication Term Term
  deriving (Eq, Show)

teAbstract :: Name -> Term -> Scope
teAbstract name =
  Scope . go 0 where
    go depth term =
      case term of
        TeFreeVariable (QualifiedName [] name') | name == name' ->
          TeBoundVariable (Index depth)
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
teInstantiate image (Scope body) =
  go 0 body where
    go depth term =
      case term of
        TeBoundVariable (Index index) | index == depth ->
          image
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

teSubstitute :: Name -> Term -> Term -> Term
teSubstitute name image term =
  teInstantiate image (teAbstract name term)
