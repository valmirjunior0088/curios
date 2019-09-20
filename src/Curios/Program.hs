module Curios.Program
  ( Statement (..)
  , Program (..)
  )
  where

import Curios.Expression
  ( Identifier (..)
  , Expression (..)
  )

data Statement =
  StProgram Program |
  StImport Identifier |
  StAssume String Expression |
  StDefine String Expression Expression
  deriving (Show)

data Program =
  Program String [Statement]
  deriving (Show)
