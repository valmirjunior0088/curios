module Core.Syntax
  ( Variable
  , wrap
  , unwrap
  , Scope
  , unbound
  , abstract
  , instantiate
  , open
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Type
  , Term (..)
  , originates
  , frees
  , commit
  )
  where

import Error (Origin)
import Data.Int (Int32)
import Control.Monad.Reader (Reader, runReader, asks, ask, local)

data Variable =
  Free String |
  Bound Int
  deriving (Show, Eq)

wrap :: String -> Variable
wrap = Free

unwrap :: Variable -> String
unwrap = \case
  Free variable -> variable
  Bound _ -> error "bound variable"

newtype Scope a = Scope a

instance Show a => Show (Scope a) where
  show (Scope scope) = "{" ++ show scope ++ "}"

instance Eq a => Eq (Scope a) where
  (==) (Scope one) (Scope other) = one == other

unbound :: a -> Scope a
unbound = Scope

abstract :: Walk a => String -> a -> Scope a
abstract target subject = Scope $ with subject $ \origin -> \case
  Free name | name == target -> asks (Local origin . Bound)
  variable -> return (Local origin variable)

open :: Walk a => String -> Scope a -> a
open name (Scope subject) = with subject $ \origin -> \case
  Bound index -> ask >>= \case
    depth | index == depth -> return (Local origin $ Free name)
    _ -> return (Local origin $ Bound index)

  variable -> return (Local origin variable)

instantiate :: Walk a => Term -> Scope a -> a
instantiate term (Scope subject) = with subject $ \origin -> \case
  Bound index -> ask >>= \case
    depth | index == depth -> return term
    _ -> return (Local origin $ Bound index)
  
  variable -> return (Local origin variable)

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
  Global Origin String |
  Local Origin Variable |

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
  Global origin _ -> origin
  Local origin _ -> origin
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

frees :: Term -> [String]
frees = \case
  Global _ _ -> []
  Local _ (Free name) -> [name]
  Local _ _ -> []
  Type _ -> []
  FunctionType _ input (Scope output) -> frees input ++ frees output
  Function _ (Scope body) -> frees body
  Apply _ function argument -> frees function ++ frees argument
  PairType _ input (Scope output) -> frees input ++ frees output
  Pair _ left right -> frees left ++ frees right
  Split _ scrutinee (Scope (Scope body)) -> frees scrutinee ++ frees body
  LabelType _ _ -> []
  Label _ _ -> []
  Match _ scrutinee branches -> frees scrutinee ++ concatMap (frees . snd) branches
  Int32Type _ -> []
  Int32 _ _ -> []
  Int32If _ _ truthy falsy -> frees truthy ++ frees falsy
  Int32BinOp _ _ left right -> frees left ++ frees right
  Int32BoolOp _ _ left right -> frees left ++ frees right
  Int32CompOp _ _ left right -> frees left ++ frees right
  Flt32Type _ -> []
  Flt32 _ _ -> []
  Flt32BinOp _ _ left right -> frees left ++ frees right
  Flt32CompOp _ _ left right -> frees left ++ frees right

commit :: Term -> Term
commit term = with term $ \origin -> \case
  Free variable -> return (Global origin variable)
  variable -> return (Local origin variable)

type Depth = Reader Int

class Walk a where
  walk :: (Origin -> Variable -> Depth Term) -> a -> Depth a

with :: Walk a => a -> (Origin -> Variable -> Depth Term) -> a
with subject action = runReader (walk action subject) 0

instance Walk Term where
  walk action = \case
    Global origin variable -> pure (Global origin variable)
    Local origin variable -> action origin variable
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
