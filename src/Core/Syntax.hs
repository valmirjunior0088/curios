module Core.Syntax
  ( Variable (Free)
  , unwrap
  , Scope
  , unbound
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Type
  , Term (..)
  , originates
  , Walk
  , abstract
  , instantiate
  , open
  , free
  )
  where

import Error (Origin)
import Data.Int (Int32)
import Data.List (intercalate)
import Control.Monad.Reader (Reader, runReader, asks, local)

data Variable =
  Free String |
  Bound Int
  deriving (Show, Eq)

unwrap :: Variable -> String
unwrap = \case
  Free name -> name
  Bound _ -> error "bound variable -- should not happen"

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

  PrimitiveType Origin PrimitiveType |
  Primitive Origin Primitive |
  Operate Origin Operation [Term]
  deriving (Eq)

instance Show Term where
  show = \case
    Global _ name -> "Global " ++ show name
    Local _ variable -> "Local (" ++ show variable ++ ")"
    Type _ -> "Type"
    FunctionType _ input scope -> "FunctionType (" ++ show input ++ ") " ++ show scope
    Function _ scope -> "Function " ++ show scope
    Apply _ function argument -> "Apply (" ++ show function ++ ") (" ++ show argument ++ ")"
    PairType _ input scope -> "PairType (" ++ show input ++ ") " ++ show scope
    Pair _ left right -> "Pair (" ++ show left ++ ") (" ++ show right ++ ")"
    Split _ scrutinee scope -> "Split (" ++ show scrutinee ++ ") " ++ show scope
    LabelType _ labels -> "LabelType [" ++ intercalate ", " labels ++ "]"
    Label _ label -> "Label " ++ show label
    Match _ scrutinee branches -> "Match (" ++ show scrutinee ++ ") [" ++ intercalate ", " (map show branches) ++ "]"
    PrimitiveType _ primitiveType -> "PrimitiveType " ++ show primitiveType
    Primitive _ primitive -> "Primitive (" ++ show primitive ++ ")"
    Operate _ operation operands -> "Operate " ++ show operation ++ " [" ++ intercalate ", " (map show operands) ++ "]"

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
    Global origin name -> pure (Global origin name)
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
    PrimitiveType origin primitiveType -> pure (PrimitiveType origin primitiveType)
    Primitive origin primitive -> pure (Primitive origin primitive)
    Operate origin operation operands -> Operate origin operation <$> mapM (walk action) operands

instance Walk a => Walk (Scope a) where
  walk action (Scope scope) = Scope <$> local succ (walk action scope)

abstract :: Walk a => String -> a -> Scope a
abstract target subject = Scope (with go subject) where
  go depth origin = \case
    Free name | name == target -> Local origin (Bound depth)
    variable -> Local origin variable

instantiate :: Walk a => Term -> Scope a -> a
instantiate term (Scope subject) = with go subject where
  go depth origin = \case
    Bound index | index == depth -> term
    variable -> Local origin variable

open :: Walk a => String -> Scope a -> a
open name (Scope subject) = with go subject where
  go depth origin = \case
    Bound index | index == depth -> Local origin (Free name)
    variable -> Local origin variable

free :: Term -> [String]
free = \case
  Global _ _ -> []
  Local _ (Free name) -> [name]
  Local _ _ -> []
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
