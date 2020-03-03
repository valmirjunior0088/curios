module Curios.Expression
  ( Name
  , QualifiedName
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

type QualifiedName =
  [Name]
  
data Atom =
  AtCharacter Char |
  AtString String |
  AtInteger Integer |
  AtRational Double |
  AtSymbol QualifiedName
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
  StImport QualifiedName |
  StDefine Name Expression Expression
  deriving (Show, Eq)

type Program =
  [Statement]

