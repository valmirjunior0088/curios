module Curios.Core.History
  (Equation
  ,History (..)
  ,hsEmpty
  ,hsInsert
  ,hsAny
  )
  where

import Curios.Core.Term (Term (..))

type Equation =
  (Term, Term)

newtype History =
  History [Equation]

hsEmpty :: History
hsEmpty =
  History []

hsInsert :: Equation -> History -> History
hsInsert item (History history) =
  History (item : history)

hsAny :: (Equation -> Bool) -> History -> Bool
hsAny predicate (History history) =
  any predicate history
