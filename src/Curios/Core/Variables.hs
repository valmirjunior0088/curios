module Curios.Core.Variables
  (Variables (..)
  ,vrEmpty
  ,vrInsert
  ,vrLookup
  ,vrNext
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

vrInsert :: Term -> Variables -> Variables
vrInsert term (Variables variables) =
  Variables (variables |> term)

vrLookup :: Natural -> Variables -> Maybe Term
vrLookup index (Variables variables) =
  Seq.lookup (naturalToInt index) variables

vrNext :: Variables -> Natural
vrNext (Variables variables) =
  intToNatural (Seq.length variables)
