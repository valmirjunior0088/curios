module Util
  ( unique
  , subset
  , (<==>)
  , (.&&.)
  , both
  , decompose
  , add
  , sub
  )
  where

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

both :: Monad m => (m a, m b) -> m (a, b)
both (left, right) = do
  left' <- left
  right' <- right
  return (left', right')

decompose :: [a] -> [(a, [a])]
decompose = \case
  [] -> []
  x : xs -> (x, xs) : decompose xs

add :: Num a => a -> a -> a
add = (+)

sub :: Num a => a -> a -> a
sub = (-)
