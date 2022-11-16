module Util
  ( (!!!)
  , unique
  , subset
  , (<==>)
  , (.&&.)
  )
  where

(!!!) :: (Show a, Eq a) => a -> [(a, b)] -> b
(!!!) target = \case
  [] -> errorWithoutStackTrace ("(!!!): nonexistent key " ++ show target)
  (key, value) : rest -> if target == key then value else target !!! rest

infixl 1 !!!

unique :: Eq a => [a] -> Bool
unique = \case
  [] -> True
  value : rest -> notElem value rest && unique rest

subset :: Eq a => [a] -> [a] -> Bool
subset ones = all (`elem` ones)

(<==>) :: Eq a => [a] -> [a] -> Bool
(<==>) ones others = subset ones others && subset others ones

infixl 1 <==>

(.&&.) :: Monad m => m Bool -> m Bool -> m Bool
(.&&.) one other = one >>= \case
  True -> other
  False -> return False

infixl 1 .&&.
