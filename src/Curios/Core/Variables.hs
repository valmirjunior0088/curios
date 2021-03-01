module Curios.Core.Variables
  (Variables (..)
  ,vrEmpty
  ,vrAllocate
  ,vrLookup
  )
  where

import Curios.Core.Term (Term (..))
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import GHC.Natural (Natural (..), naturalToInt, intToNatural)

newtype Variables =
  Variables (Seq Term)

vrEmpty :: Variables
vrEmpty =
  Variables (Seq.empty)

vrAllocate :: Term -> Variables -> (Natural, Variables)
vrAllocate term (Variables variables) =
  (intToNatural (Seq.length variables), Variables (variables |> term))

vrLookup :: Natural -> Variables -> Maybe Term
vrLookup index (Variables variables) =
  Seq.lookup (naturalToInt index) variables
