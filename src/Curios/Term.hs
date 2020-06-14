module Curios.Term
  (Primitive (..)
  ,Index (..)
  ,Scope (..)
  ,Term (..)
  ,trAbstract
  ,trInstantiate
  ,trWeaken
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  )

import Curios.Universe
  (Universe (..)
  )

import GHC.Natural
  (Natural
  )

data Primitive =
  PrCharacter |
  PrText |
  PrInteger |
  PrRational
  deriving (Eq, Show)

newtype Index =
  Index Natural
  deriving (Eq, Show)

newtype Scope =
  Scope Term
  deriving (Eq, Show)

data Term =
  TrPrimitive Primitive |
  TrLiteral Literal |
  TrFreeVariable Name |
  TrBoundVariable Index |
  TrType Universe |
  TrPiAbstraction Term Scope |
  TrLambdaAbstraction Term Scope |
  TrApplication Term Term
  deriving (Eq, Show)

trAbstract :: Name -> Term -> Scope
trAbstract name =
  Scope . go 0 where
    go depth term =
      case term of
        TrFreeVariable name' | name == name' ->
          TrBoundVariable (Index depth)

        TrPiAbstraction inputType (Scope output) ->
          TrPiAbstraction (go depth inputType) (Scope (go (depth + 1) output))

        TrLambdaAbstraction inputType (Scope output) ->
          TrLambdaAbstraction (go depth inputType) (Scope (go (depth + 1) output))

        TrApplication function argument ->
          TrApplication (go depth function) (go depth argument)

        _ ->
          term

trInstantiate :: Term -> Scope -> Term
trInstantiate image (Scope scope) =
  go 0 scope where
    go depth term =
      case term of
        TrBoundVariable (Index index) | index == depth ->
          image

        TrPiAbstraction inputType (Scope output) ->
          TrPiAbstraction (go depth inputType) (Scope (go (depth + 1) output))

        TrLambdaAbstraction inputType (Scope output) ->
          TrLambdaAbstraction (go depth inputType) (Scope (go (depth + 1) output))

        TrApplication function argument ->
          TrApplication (go depth function) (go depth argument)
          
        _ ->
          term

trWeaken :: Term -> Term
trWeaken term =
  case term of
    TrBoundVariable (Index index) -> TrBoundVariable (Index (index + 1))
    _ -> term