module Curios.Core.Variables
  (Variables (..)
  ,vrEmpty
  ,vrAllocate
  ,vrLookup
  )
  where

import Curios.Core.Term (Variable (..), Index, Term (..))
import Data.Sequence (Seq (..), (|>))
import GHC.Natural (naturalToInt, intToNatural)
import qualified Data.Sequence as Seq

newtype Variables =
  Variables (Seq Term)

vrEmpty :: Variables
vrEmpty =
  Variables (Seq.empty)

vrAllocate :: Term -> Variables -> (Variable, Variables)
vrAllocate term (Variables variables) =
  (VrQuote (intToNatural (Seq.length variables)), Variables (variables |> term))

vrLookup :: Index -> Variables -> Maybe Term
vrLookup index (Variables variables) =
  Seq.lookup (naturalToInt index) variables
