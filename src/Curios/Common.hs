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