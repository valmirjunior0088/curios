module Curios.Expression
  ( Name (..)
  , Identifier (..)
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  , Statement (..)
  , Program (..)
  )
  where

type Name =
  String

type Identifier =
  [Name]
  
data Atom =
  AtSymbol Identifier |
  AtCharacter Char |
  AtString String |
  AtInteger Integer |
  AtRational Double
  deriving (Show)

data PiBinding =
  PiBinding (Maybe Name) Expression
  deriving (Show)

data LambdaBinding =
  LambdaBinding Name (Maybe Expression)
  deriving (Show)
  
data Abstraction binding =
  Abstraction [binding] Expression
  deriving (Show)

data Expression =
  ExAtom Atom |
  ExPiAbstraction (Abstraction PiBinding) |
  ExLambdaAbstraction (Abstraction LambdaBinding) |
  ExApplication Expression [Expression]
  deriving (Show)

data Statement =
  StModule Name Program |
  StImport Identifier |
  StDefine Name Expression Expression
  deriving (Show)

type Program =
  [Statement]

