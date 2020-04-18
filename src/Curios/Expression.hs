module Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Abstraction (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  )
  where

newtype Name =
  Name String
  deriving (Show, Eq)

data QualifiedName =
  QualifiedName [Name] Name
  deriving (Show, Eq)

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double
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
  ExLiteral Literal |
  ExVariable QualifiedName |
  ExPiAbstraction (Abstraction PiBinding) |
  ExLambdaAbstraction (Abstraction LambdaBinding) |
  ExApplication Expression [Expression]
  deriving (Show, Eq)

data Statement =
  StModule Name Program |
  StImport QualifiedName |
  StDefine Name Expression Expression
  deriving (Show, Eq)

newtype Program =
  Program [Statement]
  deriving (Show, Eq)
