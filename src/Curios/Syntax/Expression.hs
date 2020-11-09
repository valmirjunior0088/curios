module Curios.Syntax.Expression
  (Name (..)
  ,Primitive (..)
  ,DependentVariable (..)
  ,Variable (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  ,pgBindings
  ,pgDefinitions
  )
  where

newtype Name =
  Name String
  deriving (Eq, Ord)

instance Show Name where
  show (Name name) =
    "\"" ++ name ++ "\"" 

data Primitive =
  PrText String |
  PrInteger Integer |
  PrRational Double
  deriving (Show, Eq)

data DependentVariable =
  DependentVariable (Maybe Name) (Maybe Name) Expression
  deriving (Show)

newtype Variable =
  Variable Name
  deriving (Show)

data Expression =
  ExName Name |
  ExPrimitive Primitive |
  ExFunctionType [DependentVariable] Expression |
  ExFunction [Variable] Expression |
  ExApplication Expression [Expression]
  deriving (Show)

data Statement =
  Statement Name Expression Expression
  deriving (Show)

newtype Program =
  Program [Statement]
  deriving (Show)

pgBindings :: Program -> [(Name, Expression)]
pgBindings (Program program) =
  map transform program where
    transform (Statement name expressionType _) = (name, expressionType)

pgDefinitions :: Program -> [(Name, Expression)]
pgDefinitions (Program program) =
  map transform program where
    transform (Statement name _ expression) = (name, expression)
