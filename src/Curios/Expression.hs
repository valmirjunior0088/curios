module Curios.Expression
  ( Name
  , Identifier
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  , Statement (..)
  , Program
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
  deriving (Show, Eq)

data PiBinding =
  PiBinding (Maybe Name) Expression
  deriving (Show, Eq)

data LambdaBinding =
  LambdaBinding Name (Maybe Expression)
  deriving (Show, Eq)
  
data Abstraction binding =
  Abstraction [binding] Expression
  deriving (Show, Eq)

data Expression =
  ExAtom Atom |
  ExPiAbstraction (Abstraction PiBinding) |
  ExLambdaAbstraction (Abstraction LambdaBinding) |
  ExApplication Expression [Expression]
  deriving (Show, Eq)

data Statement =
  StModule Name Program |
  StImport Identifier |
  StDefine Name Expression Expression
  deriving (Show, Eq)

type Program =
  [Statement]

