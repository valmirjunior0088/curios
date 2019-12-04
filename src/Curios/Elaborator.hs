module Curios.Elaborator
  ( Hole
  , Guess
  , Proof
  , ElaboratorState
  , Elaborator
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
  , Term
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
  PfPiAbstraction Proposition Proof |
  PfLambdaAbstraction Proposition Proof |
  PfApplication Proof Proof |
  PfPrimitive Primitive |
  PfLiteral Literal
  deriving (Show)

data ElaboratorState =
  ElaboratorState
    { esEnvironment :: [(Name, Term)]
    , esContext :: [Proposition]
    , esType :: Type
    , esProof :: Proof
    , esHoles :: [Hole]
    }

type Elaborator a =
  State ElaboratorState a
