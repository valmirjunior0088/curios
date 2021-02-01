module Curios.Core.History
  (History (..)
  ,hsEmpty
  ,hsInsert
  ,hsMember
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

hsMember :: Eq a => a -> History a -> Bool
hsMember item (History history) =
  any (== item) history
