module Curios.Expression
  ( Availability (..)
  , Binding (..)
  , Quantifier (..)
  , Argument (..)
  , Literal (..)
  , Expression (..)
  , Statement (..)
  , Package (..)
  )
  where

import Curios.Common
  ( Name (..)
  , Identifier (..)
  , Literal (..)
  )

data Availability =
  AvImplicit |
  AvExplicit
  deriving (Show)

data Quantifier =
  Quantifier (Maybe Name) Expression Availability
  deriving (Show)

data Binding =
  Binding Name (Maybe Expression) Availability
  deriving (Show)

data Argument =
  Argument Expression (Maybe Availability)
  deriving (Show)

data Expression =
  ExVariable Identifier |
  ExLiteral Literal |
  ExPiAbstraction [Quantifier] Expression |
  ExLambdaAbstraction [Binding] Expression |
  ExApplication Expression [Argument]
  deriving (Show)

data Statement =
  StPackage Package |
  StImport Identifier |
  StAssume String Expression |
  StDefine String Expression Expression
  deriving (Show)

data Package =
  Package Name [Statement]
  deriving (Show)
