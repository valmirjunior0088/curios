module WebAssembly.Syntax.Conventions
  ( TypeIdx (..)
  , FuncIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , ElemIdx (..)
  , DataIdx (..)
  , LocalIdx (..)
  , LabelIdx (..)
  , SecIdx (..)
  , SymIdx (..)
  , Vec (..)
  , Name (..)
  )
  where

import Data.Word (Word32)

newtype TypeIdx =
  TypeIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype FuncIdx =
  FuncIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype TableIdx =
  TableIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype MemIdx =
  MemIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype GlobalIdx =
  GlobalIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype ElemIdx =
  ElemIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype DataIdx =
  DataIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype LocalIdx =
  LocalIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype LabelIdx =
  LabelIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype SecIdx =
  SecIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype SymIdx =
  SymIdx Word32
  deriving (Show, Num, Enum, Eq, Ord, Real, Integral)

newtype Vec a =
  Vec [a]
  deriving (Show, Eq, Semigroup, Monoid, Functor, Applicative, Foldable)

newtype Name =
  Name String
  deriving (Show, Eq)
