module Curios.Core.History
  (History (..)
  ,hsEmpty
  ,hsInsert
  ,hsAny
  )
  where

import Curios.Core.Term (Term (..))

newtype History =
  History [(Term, Term)]

hsEmpty :: History
hsEmpty =
  History []

hsInsert :: (Term, Term) -> History -> History
hsInsert item (History history) =
  History (item : history)

hsAny :: ((Term, Term) -> Bool) -> History -> Bool
hsAny predicate (History history) =
  any predicate history
