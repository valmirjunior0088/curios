module Curios.Core.Environment
  (Environment (..)
  ,enEmpty
  ,enInsert
  ,enLookup
  ,enLength
  )
  where

import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq

import GHC.Natural (Natural (..), naturalToInt, intToNatural)
import Curios.Core.Term (Term (..))

newtype Environment =
  Environment (Seq Term)
  deriving (Show)

enEmpty :: Environment
enEmpty =
  Environment (Seq.empty)

enInsert :: Term -> Environment -> Environment
enInsert term (Environment environment) =
  Environment (environment |> term)

enLookup :: Natural -> Environment -> Maybe Term
enLookup index (Environment environment) =
  Seq.lookup (naturalToInt index) environment

enLength :: Environment -> Natural
enLength (Environment environment) =
  intToNatural (Seq.length environment)
