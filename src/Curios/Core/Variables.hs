module Curios.Core.Variables
  (Variables (..)
  ,vrEmpty
  ,vrAllocate
  ,vrLookup
  )
  where

import Curios.Core.Term (Variable (..), Index, Type)
import Data.Sequence (Seq, (|>))
import GHC.Natural (naturalToInt, intToNatural)
import qualified Data.Sequence as Seq

newtype Variables =
  Variables (Seq Type)

vrEmpty :: Variables
vrEmpty =
  Variables (Seq.empty)

vrAllocate :: Type -> Variables -> (Variable, Variables)
vrAllocate term (Variables variables) =
  (VrQuote (intToNatural (Seq.length variables)), Variables (variables |> term))

vrLookup :: Index -> Variables -> Maybe Type
vrLookup index (Variables variables) =
  Seq.lookup (naturalToInt index) variables
