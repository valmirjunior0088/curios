module Curios.Term
  (Primitive (..)
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
  (Natural (..)
  )

data Primitive =
  PrCharacter |
  PrText |
  PrInteger |
  PrRational
  deriving (Eq, Show)

newtype Scope =
  Scope Term
  deriving (Eq, Show)

data Term =
  TrPrimitive Primitive |
  TrLiteral Literal |
  TrFreeVariable Name |
  TrBoundVariable Natural |
  TrType Universe |
  TrAbstractionType Term Scope |
  TrAbstraction Term Scope |
  TrApplication Term Term
  deriving (Eq, Show)

trUpdateVariables :: (Natural -> Name -> Term) -> (Natural -> Natural -> Term) -> Term -> Term
trUpdateVariables handleFreeVariable handleBoundVariable =
  go 0 where
    go depth term =
      case term of
        TrFreeVariable name ->
          handleFreeVariable depth name
        
        TrBoundVariable index ->
          handleBoundVariable depth index

        TrAbstractionType inputType (Scope output) ->
          TrAbstractionType (go depth inputType) (Scope (go (depth + 1) output))

        TrAbstraction inputType (Scope output) ->
          TrAbstraction (go depth inputType) (Scope (go (depth + 1) output))

        TrApplication function argument ->
          TrApplication (go depth function) (go depth argument)

        _ ->
          term


trAbstract :: Name -> Term -> Scope
trAbstract name term =
  Scope (trUpdateVariables handleFreeVariable handleBoundVariable term) where
    handleFreeVariable depth name' =
      if name == name'
        then TrBoundVariable depth
        else TrFreeVariable name'

    handleBoundVariable _ index =
      TrBoundVariable index

trInstantiate :: Term -> Scope -> Term
trInstantiate image (Scope term) =
  trUpdateVariables handleFreeVariable handleBoundVariable term where
    handleFreeVariable _ name =
      TrFreeVariable name

    handleBoundVariable depth index =
      if depth == index
        then image
        else TrBoundVariable index

trWeaken :: Term -> Term
trWeaken term =
  trUpdateVariables handleFreeVariable handleBoundVariable term where
    handleFreeVariable _ name =
      TrFreeVariable name

    handleBoundVariable _ index =
      TrBoundVariable (index + 1)