module Curios.Expression
  ( Availability (..)
  , Binding (..)
  , Quantifier (..)
  , Argument (..)
  , Literal (..)
  , Identifier (..)
  , Expression (..)
  )
  where

data Availability =
  AvImplicit |
  AvExplicit
  deriving (Show)

data Quantifier =
  Quantifier (Maybe String) Expression Availability
  deriving (Show)

data Binding =
  Binding String (Maybe Expression) Availability
  deriving (Show)

data Argument =
  Argument Expression (Maybe Availability)
  deriving (Show)

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double
  deriving (Show)

data Identifier =
  Identifier [String]
  deriving (Show)

data Expression =
  ExPiAbstraction [Quantifier] Expression |
  ExLambdaAbstraction [Binding] Expression |
  ExApplication Expression [Argument] |
  ExLiteral Literal |
  ExVariable Identifier
  deriving (Show)
