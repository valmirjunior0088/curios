module Inter.Syntax
  ( Variable
  , wrap
  , unwrap
  , Scope
  , unbound
  , abstract
  , instantiate
  , count
  , Atom (..)
  , free
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Target (..)
  , Expression (..)
  , Sequence (..)
  , construct
  , consumes
  , Program (..)
  )
  where

import Util (add, sub)
import Core.Syntax (BinOp (..), BoolOp (..), CompOp (..))
import Data.Int (Int32)
import Data.List (elemIndex)
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader (..), Reader, runReader, asks)
import GHC.Generics (Generic)

data Variable =
  Free String |
  Bound Int
  deriving (Show)

wrap :: String -> Variable
wrap = Free

unwrap :: Variable -> String
unwrap = \case
  Free variable -> variable
  Bound _ -> error "bound variable"

data Scope a = Scope Int a

instance Show a => Show (Scope a) where
  show (Scope quantity scope) = "{ " ++ show quantity ++ " | " ++ show scope ++ " }"

unbound :: a -> Scope a
unbound = Scope 0

abstract :: Walk a => [String] -> a -> Scope a
abstract targets expression = Scope (length targets) $ with expression $ \case
  Free variable -> case variable `elemIndex` targets of
    Nothing -> return (Variable $ Free variable)
    Just index -> asks (Variable . Bound . add index)
  
  variable -> return (Variable variable)

instantiate :: Walk a => [Atom] -> Scope a -> a
instantiate atoms (Scope _ scope) = with scope $ \case
  Bound variable -> asks (sub variable) <&> \case
    index | index >= 0 -> atoms !! index
    _ -> Variable (Bound variable)

  variable -> return (Variable variable)

count :: Scope a -> Int
count (Scope quantity _) = quantity

data Atom =
  Null |
  Variable Variable
  deriving (Show)

free :: String -> Atom
free = Variable . Free

data Target = Target
  { block :: String
  , atoms :: [Atom]
  }
  deriving (Show, Generic)

data Expression =
  Pure Atom |
  Jump Target |
  ClosureAlloc String [Atom] |
  ClosureEnter Atom [Atom] |
  StructAlloc [Atom] |
  StructSelect Atom Int32 |
  Int32Alloc Int32 |
  Int32If Atom Target Target |
  Int32Match Atom [(Int32, Target)] |
  Int32BinOp BinOp Atom Atom |
  Int32BoolOp BoolOp Atom Atom |
  Int32CompOp CompOp Atom Atom |
  Flt32Alloc Float |
  Flt32BinOp BinOp Atom Atom |
  Flt32CompOp CompOp Atom Atom
  deriving (Show)

data Sequence = Sequence
  { expression :: Expression
  , continuation :: Maybe (Scope Sequence)
  }

construct :: [(String, Expression)] -> Expression -> Sequence
construct expressions expression = foldr go (Sequence expression Nothing) expressions where
  go (name, body) continuation = Sequence body (Just $ abstract [name] continuation)

class Consumes a where
  consumes :: a -> [String]

instance Consumes Atom where
  consumes = \case
    Null -> []
    Variable (Bound _) -> []
    Variable (Free name) -> [name]

instance Consumes Target where
  consumes Target { atoms } = concatMap consumes atoms

instance Consumes Expression where
  consumes = \case
    Pure atom -> consumes atom
    Jump target -> consumes target
    ClosureAlloc _ atoms -> concatMap consumes atoms
    ClosureEnter _ atoms -> concatMap consumes atoms
    StructAlloc atoms -> concatMap consumes atoms
    StructSelect _ _ -> []
    Int32Alloc _ -> []
    Int32If _ _ _ -> []
    Int32Match _ _ -> []
    Int32BinOp _ _ _ -> []
    Int32BoolOp _ _ _ -> []
    Int32CompOp _ _ _ -> []
    Flt32Alloc _ -> []
    Flt32BinOp _ _ _ -> []
    Flt32CompOp _ _ _ -> []

instance Consumes Sequence where
  consumes Sequence { expression, continuation } =
    consumes expression ++ consumes continuation

instance Consumes a => Consumes (Maybe a) where
  consumes = maybe [] consumes

instance Consumes a => Consumes (Scope a) where
  consumes (Scope _ scope) = consumes scope

data Program = Program
  { blocks :: [(String, Scope Sequence)]
  , closures :: [(String, Scope (Scope Target))]
  }
  deriving (Generic)

instance Semigroup Program where
  (<>) (Program blocks closures) (Program blocks' closures') =
    Program (blocks <> blocks') (closures <> closures')

instance Monoid Program where
  mempty = Program { blocks = [], closures = [] }

type Depth = Reader Int

class Walk a where
  walk :: (Variable -> Depth Atom) -> a -> Depth a

with :: Walk a => a -> (Variable -> Depth Atom) -> a
with subject action = runReader (walk action subject) 0

instance Walk Atom where
  walk action = \case
    Null -> return Null
    Variable variable -> action variable

instance Walk Target where
  walk action Target { block, atoms } = Target block <$> mapM (walk action) atoms

instance Walk Expression where
  walk action = \case
    Pure atom -> Pure <$> walk action atom
    Jump target -> Jump <$> walk action target
    ClosureAlloc name atoms -> ClosureAlloc name <$> mapM (walk action) atoms
    ClosureEnter atom atoms -> ClosureEnter <$> walk action atom <*> mapM (walk action) atoms
    StructAlloc atoms -> StructAlloc <$> mapM (walk action) atoms
    StructSelect atom index -> StructSelect <$> walk action atom <*> pure index
    Int32Alloc value -> pure (Int32Alloc value)
    Int32Match atom branches -> Int32Match <$> walk action atom <*> mapM (mapM $ walk action) branches
    Int32If atom truthy falsy -> Int32If <$> walk action atom <*> walk action truthy <*> walk action falsy
    Int32BinOp op left right -> Int32BinOp op <$> walk action left <*> walk action right
    Int32BoolOp op left right -> Int32BoolOp op <$> walk action left <*> walk action right
    Int32CompOp op left right -> Int32CompOp op <$> walk action left <*> walk action right
    Flt32Alloc value -> pure (Flt32Alloc value)
    Flt32BinOp op left right -> Flt32BinOp op <$> walk action left <*> walk action right
    Flt32CompOp op left right -> Flt32CompOp op <$> walk action left <*> walk action right

instance Walk Sequence where
  walk action Sequence { expression, continuation } =
    Sequence <$> walk action expression <*> walk action continuation

instance Walk a => Walk (Maybe a) where
  walk action = \case
    Nothing -> pure Nothing
    Just scope -> Just <$> walk action scope

instance Walk a => Walk (Scope a) where
  walk action (Scope depth scope) = Scope depth <$> local (add depth) (walk action scope)
