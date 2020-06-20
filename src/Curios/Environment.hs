module Curios.Environment
  (Environment (..)
  ,enEmpty
  ,enInsert
  ,enLookup
  ,enWeaken
  )
  where
  
import Prelude hiding
  (lookup
  )

import Curios.Term
  (Term (..)
  ,trWeaken
  )

import Data.Sequence
  (Seq (..)
  ,empty
  ,(<|)
  ,lookup
  )

import GHC.Natural
  (Natural (..)
  ,naturalToInt
  )

newtype Environment =
  Environment (Seq Term)
  deriving (Show)

enEmpty :: Environment
enEmpty =
  Environment empty

enInsert :: Term -> Environment -> Environment
enInsert term (Environment environment) =
  Environment (term <| environment)

enLookup :: Natural -> Environment -> Either String Term
enLookup index (Environment environment) =
  case lookup (naturalToInt index) environment of
    Nothing -> Left ("Bound variable `" ++ show index ++ "` does not exist in the target environment")
    Just term -> Right term

enWeaken :: Environment -> Environment
enWeaken (Environment environment) =
  Environment (fmap trWeaken environment)