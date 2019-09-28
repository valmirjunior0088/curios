module Curios.Common
  ( Identifier (..)
  , Name (..)
  )
  where

type Name =
  String

data Identifier =
  Identifier [Name]
  deriving (Show)

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double
  deriving (Show)
