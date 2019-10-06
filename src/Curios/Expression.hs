module Curios.Expression
  ( Name (..)
  , Availability (..)
  , Binding (..)
  , Quantifier (..)
  , Argument (..)
  , Literal (..)
  , Identifier (..)
  , Expression (..)
  , Statement (..)
  , Program (..)
  )
  where

data Name =
  Name String
  deriving (Show)

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

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double
  deriving (Show)

data Identifier =
  Identifier [Name]
  deriving (Show)

data Expression =
  ExPiAbstraction [Quantifier] Expression |
  ExLambdaAbstraction [Binding] Expression |
  ExApplication Expression [Argument] |
  ExLiteral Literal |
  ExVariable Identifier
  deriving (Show)

data Statement =
  StPackage Name Program |
  StImport Identifier |
  StAssume Name Expression |
  StDefine Name Expression Expression |
  StAlias Name Expression
  deriving (Show)

data Program =
  Program [Statement]
  deriving (Show)
