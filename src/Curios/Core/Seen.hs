module Curios.Core.Seen
  (Seen (..)
  ,snEmpty
  ,snInsert
  ,snMember
  )
  where

newtype Seen a =
  Seen [a]

snEmpty :: Seen a
snEmpty =
  Seen []

snInsert :: a -> Seen a -> Seen a
snInsert item (Seen seen) =
  Seen (item : seen)

snMember :: Eq a => a -> Seen a -> Bool
snMember item (Seen seen) =
  any (== item) seen
