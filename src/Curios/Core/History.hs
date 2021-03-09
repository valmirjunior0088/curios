module Curios.Core.History
  (History (..)
  ,hsEmpty
  ,hsInsert
  ,hsAny
  )
  where

newtype History a =
  History [a]

hsEmpty :: History a
hsEmpty =
  History []

hsInsert :: a -> History a -> History a
hsInsert item (History history) =
  History (item : history)

hsAny :: (a -> Bool) -> History a -> Bool
hsAny predicate (History history) =
  any predicate history
