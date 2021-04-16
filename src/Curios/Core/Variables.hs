module Curios.Core.Variables
  ( Variables (..)
  , vrEmpty
  , vrAllocate
  , vrLookup
  , vrDepth
  )
  where

import Curios.Core (Variable (..), Index, Type)
import GHC.Natural (Natural, naturalToInt, intToNatural)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

newtype Variables =
  Variables (Seq Type)

depth :: Seq Type -> Natural
depth variables =
  intToNatural (Seq.length variables)

vrEmpty :: Variables
vrEmpty =
  Variables Seq.empty

vrAllocate :: Type -> Variables -> (Variable, Variables)
vrAllocate termType (Variables variables) =
  (VrQuote (depth variables), Variables (variables |> termType))

vrLookup :: Index -> Variables -> Maybe Type
vrLookup index (Variables variables) =
  Seq.lookup (naturalToInt index) variables

vrDepth :: Variables -> Natural
vrDepth (Variables variables) =
  depth variables
