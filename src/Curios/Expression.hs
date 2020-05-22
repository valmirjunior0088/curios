module Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  )
  where

data Literal =
  LiCharacter Char |
  LiText String |
  LiInteger Integer |
  LiRational Double
  deriving (Eq, Show)

newtype Name =
  Name String
  deriving (Eq, Show)

data QualifiedName =
  QualifiedName [Name] Name
  deriving (Eq, Show)

data PiBinding =
  PiBinding (Maybe Name) Expression
  deriving (Eq, Show)

data LambdaBinding =
  LambdaBinding Name (Maybe Expression)
  deriving (Eq, Show)

data Expression =
  ExLiteral Literal |
  ExVariable QualifiedName |
  ExPiAbstraction [PiBinding] Expression |
  ExLambdaAbstraction [LambdaBinding] Expression|
  ExApplication Expression [Expression]
  deriving (Eq, Show)

data Statement =
  StModule Name Program |
  StImport [QualifiedName] |
  StDefine Name Expression Expression
  deriving (Eq, Show)

newtype Program =
  Program [Statement]
  deriving (Eq, Show)
