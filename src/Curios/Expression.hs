module Curios.Expression
  ( Name (..)
  , Identifier (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Literal (..)
  , Expression (..)
  , Statement (..)
  , Program (..)
  )
  where

data Name =
  Name String
  deriving (Show)

data Identifier =
  Identifier [Name]
  deriving (Show)

data PiBinding =
  PiBinding (Maybe Name) Expression
  deriving (Show)

data LambdaBinding =
  LambdaBinding Name (Maybe Expression)
  deriving (Show)

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double
  deriving (Show)

data Expression =
  ExVariable Identifier |
  ExPiAbstraction [PiBinding] Expression |
  ExLambdaAbstraction [LambdaBinding] Expression |
  ExApplication Expression [Expression] |
  ExLiteral Literal
  deriving (Show)

data Statement =
  StPackage Name Program |
  StImport Identifier |
  StAssume Name Expression |
  StDefine Name Expression Expression
  deriving (Show)

data Program =
  Program [Statement]
  deriving (Show)
