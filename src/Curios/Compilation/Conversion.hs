module Curios.Compilation.Conversion
  ( Name
  , Literal (..)
  , Operation (..)
  , Variable (..)
  , Term (..)
  , Item (..)
  , convert
  )
  where

import Data.List (elemIndex)
import Control.Monad.State (State, runState, evalState, get, put)

import Curios.Compilation.Erasure (Name, Index, Literal (..))
import qualified Curios.Compilation.Erasure as Erasure

data Operation =
  OpInt32Sum Term Term |
  OpFlt32Sum Term Term
  deriving (Show)

data Variable =
  VrEnvironment Int |
  VrArgument
  deriving (Show)

data Term =
  TrLiteral Literal |
  TrOperation Operation |
  TrReference String |
  TrVariable Variable |
  TrFunction [Variable] Term |
  TrApplication Term Term |
  TrNull
  deriving (Show)

type Conversion =
  State [Index]

runConversion :: Conversion a -> (a, [Index])
runConversion action =
  runState action []

evalConversion :: Conversion a -> a
evalConversion action =
  evalState action []

rebind :: Index -> Conversion Variable
rebind variable =
  if variable == 0 then return VrArgument else do
    variables <- get

    case elemIndex (pred variable) variables of
      Nothing -> do
        put (variables ++ [pred variable])
        return (VrEnvironment $ length variables)
        
      Just index ->
        return (VrEnvironment index)

unwrap :: Erasure.Term -> Conversion Term
unwrap term =
  case term of
    Erasure.TrLiteral literal ->
      return (TrLiteral literal)

    Erasure.TrOperation (Erasure.OpInt32Sum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrOperation $ OpInt32Sum left' right')

    Erasure.TrOperation (Erasure.OpFlt32Sum left right) -> do
      left' <- unwrap left
      right' <- unwrap right
      return (TrOperation $ OpFlt32Sum left' right')

    Erasure.TrReference reference ->
      return (TrReference reference)

    Erasure.TrVariable variable -> do
      variable' <- rebind variable
      return (TrVariable variable')

    Erasure.TrFunction output -> do
      let (term', variables) = runConversion (unwrap output)
      variables' <- mapM rebind variables
      return (TrFunction variables' term')

    Erasure.TrApplication function argument -> do
      function' <- unwrap function
      argument' <- unwrap argument
      return (TrApplication function' argument')

    Erasure.TrNull ->
      return TrNull

data Item =
  Item Name Term

convert :: Erasure.Item -> Item
convert (Erasure.Item name term) =
  Item name (evalConversion $ unwrap term)
