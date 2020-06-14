module Curios.Universe
  (Universe (..)
  ,unNext
  ,unMax
  )
  where

import GHC.Natural
  (Natural (..)
  )

newtype Universe =
  Universe Natural
  deriving (Show)

instance Eq Universe where
  (Universe universe) == (Universe universe') =
    universe <= universe'

unNext :: Universe -> Universe
unNext (Universe universe) =
  Universe (universe + 1)

unMax :: Universe -> Universe -> Universe
unMax (Universe universe) (Universe universe') =
  Universe (max universe universe')