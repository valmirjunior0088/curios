module Core.Syntax
  ( Variable (Global, LocalFree)
  , Scope
  , unbound
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Type
  , Term (..)
  , originates
  , capture
  , abstract
  , instantiate
  , open
  , free
  )
  where

import Error (Origin)
import Data.Int (Int32)
import Control.Monad.Reader (Reader, runReader, asks, local)

data Variable =
  Global String |
  LocalFree String |
  LocalBound Int
  deriving (Show, Eq)

newtype Scope a = Scope a

instance Show a => Show (Scope a) where
  show (Scope scope) = "{" ++ show scope ++ "}"

instance Eq a => Eq (Scope a) where
  (==) (Scope one) (Scope other) = one == other

unbound :: a -> Scope a
unbound = Scope

data PrimitiveType =
  Int32Type |
  Flt32Type
  deriving (Show, Eq)

data Primitive =
  Int32 Int32 |
  Flt32 Float
  deriving (Show, Eq)

data Operation =
  Int32Add |
  Flt32Add
  deriving (Show, Eq)

type Type = Term

data Term =
  Variable Origin Variable |

  Type Origin |

  FunctionType Origin Type (Scope Type) |
  Function Origin (Scope Term) |
  Apply Origin Term Term |

  PairType Origin Type (Scope Type) |
  Pair Origin Term Term |
  Split Origin Term (Scope (Scope Term)) |

  LabelType Origin [String] |
  Label Origin String |
  Match Origin Term [(String, Term)] |

  PrimitiveType Origin PrimitiveType |
  Primitive Origin Primitive |
  Operate Origin Operation [Term]
  deriving (Show, Eq)

originates :: Term -> Origin
originates = \case
  Variable origin _ -> origin
  Type origin -> origin
  FunctionType origin _ _ -> origin
  Function origin _ -> origin
  Apply origin _ _ -> origin
  PairType origin _ _ -> origin
  Pair origin _ _ -> origin
  Split origin _ _ -> origin
  LabelType origin _ -> origin
  Label origin _ -> origin
  Match origin _ _ -> origin
  PrimitiveType origin _ -> origin
  Primitive origin _ -> origin
  Operate origin _ _ -> origin

type Depth = Reader Int

class Walk a where
  walk :: (Origin -> Variable -> Depth Term) -> a -> Depth a

with :: Walk a => (Int -> Origin -> Variable -> Term) -> a -> a
with action subject = runReader (walk go subject) 0 where
  go origin variable = asks action <*> pure origin <*> pure variable

instance Walk Term where
  walk action = \case
    Variable origin variable -> action origin variable
    Type origin -> pure (Type origin)
    FunctionType origin input scope -> FunctionType origin <$> walk action input <*> walk action scope
    Function origin body -> Function origin <$> walk action body
    Apply origin function argument -> Apply origin <$> walk action function <*> walk action argument
    PairType origin input scope -> PairType origin <$> walk action input <*> walk action scope
    Pair origin left right -> Pair origin <$> walk action left <*> walk action right
    Split origin scrutinee body -> Split origin <$> walk action scrutinee <*> walk action body
    LabelType origin set -> pure (LabelType origin set)
    Label origin label -> pure (Label origin label)
    Match origin scrutinee branches -> Match origin <$> walk action scrutinee <*> mapM (mapM $ walk action) branches
    PrimitiveType origin primitiveType -> pure (PrimitiveType origin primitiveType)
    Primitive origin primitive -> pure (Primitive origin primitive)
    Operate origin operation operands -> Operate origin operation <$> mapM (walk action) operands

instance Walk a => Walk (Scope a) where
  walk action (Scope scope) = Scope <$> local succ (walk action scope)

capture :: Walk a => String -> a -> Scope a
capture target subject = Scope (with go subject) where
  go depth origin = \case
    Global name | name == target -> Variable origin (LocalBound depth)
    variable -> Variable origin variable

abstract :: Walk a => String -> a -> Scope a
abstract target subject = Scope (with go subject) where
  go depth origin = \case
    LocalFree name | name == target -> Variable origin (LocalBound depth)
    variable -> Variable origin variable

instantiate :: Walk a => Term -> Scope a -> a
instantiate term (Scope subject) = with go subject where
  go depth origin = \case
    LocalBound index | index == depth -> term
    variable -> Variable origin variable

open :: Walk a => String -> Scope a -> a
open name (Scope subject) = with go subject where
  go depth origin = \case
    LocalBound index | index == depth -> Variable origin (LocalFree name)
    variable -> Variable origin variable

free :: Term -> [String]
free = \case
  Variable _ (LocalFree name) -> [name]
  Variable _ _ -> []
  Type _ -> []
  FunctionType _ input (Scope output) -> free input ++ free output
  Function _ (Scope body) -> free body
  Apply _ function argument -> free function ++ free argument
  PairType _ input (Scope output) -> free input ++ free output
  Pair _ left right -> free left ++ free right
  Split _ scrutinee (Scope (Scope body)) -> free scrutinee ++ free body
  LabelType _ _ -> []
  Label _ _ -> []
  Match _ scrutinee branches -> free scrutinee ++ concatMap (free . snd) branches
  PrimitiveType _ _ -> []
  Primitive _ _ -> []
  Operate _ _ operands -> concatMap free operands
