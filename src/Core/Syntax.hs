module Core.Syntax
  ( Variable (Global, LocalFree)
  , Scope
  , unbound
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
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
import Control.Monad.Reader (Reader, runReader, ask, local)

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

data BinOp =
  Add |
  Sub |
  Mul |
  Div
  deriving (Show, Eq)

data BoolOp =
  And |
  Or
  deriving (Show, Eq)

data CompOp =
  Eq |
  Ne |
  Lt |
  Le |
  Gt |
  Ge
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

  Int32Type Origin |
  Int32 Origin Int32 |
  Int32If Origin Term Term Term |
  Int32BinOp Origin BinOp Term Term |
  Int32BoolOp Origin BoolOp Term Term |
  Int32CompOp Origin CompOp Term Term |

  Flt32Type Origin |
  Flt32 Origin Float |
  Flt32BinOp Origin BinOp Term Term |
  Flt32CompOp Origin CompOp Term Term
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
  Int32Type origin -> origin
  Int32 origin _ -> origin
  Int32If origin _ _ _ -> origin
  Int32BinOp origin _ _ _ -> origin
  Int32BoolOp origin _ _ _ -> origin
  Int32CompOp origin _ _ _ -> origin
  Flt32Type origin -> origin
  Flt32 origin _ -> origin
  Flt32BinOp origin _ _ _ -> origin
  Flt32CompOp origin _ _ _ -> origin

type Depth = Reader Int

class Walk a where
  walk :: (Origin -> Variable -> Depth Term) -> a -> Depth a

with :: Walk a => (Int -> Origin -> Variable -> Term) -> a -> a
with action subject = runReader (walk go subject) 0 where
  go origin variable = do depth <- ask; return (action depth origin variable)

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
    Int32Type origin -> pure (Int32Type origin)
    Int32 origin value -> pure (Int32 origin value)
    Int32If origin scrutinee truthy falsy -> Int32If origin <$> walk action scrutinee <*> walk action truthy <*> walk action falsy
    Int32BinOp origin op left right -> Int32BinOp origin op <$> walk action left <*> walk action right
    Int32BoolOp origin op left right -> Int32BoolOp origin op <$> walk action left <*> walk action right
    Int32CompOp origin op left right -> Int32CompOp origin op <$> walk action left <*> walk action right
    Flt32Type origin -> pure (Flt32Type origin)
    Flt32 origin value -> pure (Flt32 origin value)
    Flt32BinOp origin operation left right -> Flt32BinOp origin operation <$> walk action left <*> walk action right
    Flt32CompOp origin operation left right -> Flt32CompOp origin operation <$> walk action left <*> walk action right

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
  Int32Type _ -> []
  Int32 _ _ -> []
  Int32If _ _ truthy falsy -> free truthy ++ free falsy
  Int32BinOp _ _ left right -> free left ++ free right
  Int32BoolOp _ _ left right -> free left ++ free right
  Int32CompOp _ _ left right -> free left ++ free right
  Flt32Type _ -> []
  Flt32 _ _ -> []
  Flt32BinOp _ _ left right -> free left ++ free right
  Flt32CompOp _ _ left right -> free left ++ free right
