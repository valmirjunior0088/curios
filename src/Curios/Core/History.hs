module Curios.Core.History
  (Equation
  ,History (..)
  ,hsEmpty
  ,hsInsert
  ,hsAny
  )
  where

import Curios.Core (Term)

type Equation =
  (Term, Term)

newtype History =
  History [Equation]

hsEmpty :: History
hsEmpty =
  History []

hsInsert :: Equation -> History -> History
hsInsert equation (History history) =
  History (equation : history)

hsAny :: (Equation -> Bool) -> History -> Bool
hsAny predicate (History history) =
  any predicate history
