module Curios.Elaborator
  ( Hole
  , Guess
  , Proof
  )
  where

import Curios.Expression
  ( Name
  , Identifier
  , Literal
  )

import Curios.Term
  ( Type
  , Primitive
  )

import Control.Monad.State
  ( State
  )

data Hole =
  Hole Name Type
  deriving (Show)

data Guess =
  Guess Name Proof
  deriving (Show)

data Proof =
  PfHole Hole |
  PfGuess Guess |
  PfFreeVariable Identifier |
  PfBoundVariable Integer |
  PfType Integer |
  PfPiAbstraction Type Proof |
  PfLambdaAbstraction Type Proof |
  PfApplication Proof Proof |
  PfPrimitive Primitive |
  PfLiteral Literal
  deriving (Show)

-- This is missing stuff like context and environment
data ElaboratorState =
  ElaboratorState
    { esProof :: Proof
    , esHoles :: [Hole]
    }

type Elaborator a =
  State ElaboratorState a
