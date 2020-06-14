module Curios.Environment
  (Environment (..)
  ,enEmpty
  ,enInsert
  ,enLookup
  )
  where
  
import Prelude hiding
  (lookup
  )

import Curios.Term
  (Index (..)
  ,Term (..)
  )

import Data.Sequence
  (Seq (..)
  ,empty
  ,(<|)
  ,lookup
  )

import GHC.Natural
  (naturalToInt
  )

newtype Environment =
  Environment (Seq Term)

enEmpty :: Environment
enEmpty =
  Environment empty

enInsert :: Term -> Environment -> Environment
enInsert term (Environment environment) =
  Environment (term <| environment)

enLookup :: Index -> Environment -> Either String Term
enLookup (Index index) (Environment environment) =
  case lookup (naturalToInt index) environment of
    Nothing -> Left ("Bound variable `" ++ show index ++ "` does not exist in the target environment")
    Just term -> Right term