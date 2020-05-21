module Curios.Identifier
  (Identifier (..)
  ,idNew
  )
  where

import Data.Unique
  (Unique
  ,newUnique
  )

newtype Identifier =
  Identifier Unique
  deriving (Eq)

instance Show Identifier where
  show _ =
    "?"

idNew :: IO Identifier
idNew =
  fmap Identifier newUnique