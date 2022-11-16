module Intermediate.Syntax
  ( Atom (..)
  , Expression (..)
  , Sequence (..)
  , bind
  , Closure (..)
  , Block (..)
  , Program (..)
  , emptyProgram
  , mentions
  , consumes
  )
  where

import Data.Int (Int32)
import GHC.Generics (Generic)

data Atom =
  Environmental String |
  Local String |
  Null
  deriving (Show, Eq)

data Expression =
  Int32Alloc Int32 |
  Int32Add Atom Atom |
  Flt32Alloc Float |
  Flt32Add Atom Atom |
  Pure Atom |
  BlockCall String [Atom] |
  Int32Match Atom [(Int32, Expression)] |
  ClosureAlloc String [Atom] |
  ClosureEnter Atom [Atom] |
  StructAlloc [Atom] |
  StructSelect Atom Int32
  deriving (Show)

data Sequence =
  Bind String Expression Sequence |
  Tail Expression
  deriving (Show)

bind :: String -> Sequence -> Sequence -> Sequence
bind name body rest = case body of
  Bind name' body' rest' -> Bind name' body' (bind name rest' rest)
  Tail body' -> Bind name body' rest

data Closure = Closure
  { environment :: [String]
  , parameters :: [String]
  , body :: Sequence
  }
  deriving (Show, Generic)

data Block = Block
  { parameters :: [String]
  , body :: Sequence
  }
  deriving (Show, Generic)

data Program = Program
  { closures :: [(String, Closure)]
  , blocks :: [(String, Block)]
  }
  deriving (Show, Generic)

emptyProgram :: Program
emptyProgram = Program
  { closures = []
  , blocks = []
  }

class Mentions a where
  mentions :: a -> [Atom]

instance Mentions Expression where
  mentions = \case
    Int32Alloc _ -> []
    Int32Add _ _ -> []
    Flt32Alloc _ -> []
    Flt32Add _ _ -> []
    Pure atom -> [atom]
    BlockCall _ atoms -> atoms
    Int32Match _ _ -> []
    ClosureAlloc _ atoms -> atoms
    ClosureEnter _ atoms -> atoms
    StructAlloc atoms -> atoms
    StructSelect _ _ -> []

instance Mentions Sequence where
  mentions = \case
    Bind _ expression rest -> mentions expression ++ mentions rest
    Tail expression -> mentions expression

consumes :: Mentions a => a -> String -> Bool
consumes value name = Local name `elem` mentions value
