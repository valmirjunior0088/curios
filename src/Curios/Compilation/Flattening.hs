module Curios.Compilation.Flattening
  ( Literal (..)
  , Operation (..)
  , Variable (..)
  , Term (..)
  , Definition (..)
  , Abstraction (..)
  , Item (..)
  , flatten
  )
  where

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (Writer, runWriter, tell)

import Curios.Compilation.Conversion (Name, Literal (..), Variable (..), Operation (..))
import qualified Curios.Compilation.Conversion as Conversion

data Term =
  TrLiteral Literal |
  TrOperation Operation [Term] |
  TrReference Name |
  TrVariable Variable |
  TrClosure Name [Variable] |
  TrApplication Term Term |
  TrNil
  deriving (Show)

data Definition =
  Definition Name Term

data Abstraction =
  Abstraction Name Int Term
  deriving (Show)

type Flattening =
  ReaderT Name (StateT Int (Writer [Abstraction]))

runFlattening :: Flattening a -> Name -> (a, [Abstraction])
runFlattening action name =
  runWriter (evalStateT (runReaderT action name) 0)

mangleDefinition :: Name -> Name
mangleDefinition name =
  name ++ "$def"

mangleAbstraction :: Name -> Int -> Name
mangleAbstraction name index =
  name ++ "$abs_" ++ show index

fresh :: Flattening Name
fresh = do
  name <- ask
  index <- get
  put (succ index)
  return (mangleAbstraction name index)

unwrap :: Conversion.Term -> Flattening Term
unwrap term =
  case term of
    Conversion.TrLiteral literal ->
      return (TrLiteral literal)

    Conversion.TrOperation operation arguments -> do
      arguments' <- mapM unwrap arguments
      return (TrOperation operation arguments')

    Conversion.TrReference reference ->
      return (TrReference $ mangleDefinition reference)

    Conversion.TrVariable variable -> 
      return (TrVariable variable)

    Conversion.TrFunction variables scope -> do
      body <- unwrap scope
      name <- fresh
      tell [Abstraction name (length variables) body]
      return (TrClosure name variables)

    Conversion.TrApplication function argument -> do
      function' <- unwrap function
      argument' <- unwrap argument
      return (TrApplication function' argument')

    Conversion.TrNil ->
      return TrNil

data Item =
  Item Definition [Abstraction]

flatten :: Conversion.Item -> Item
flatten (Conversion.Item name term) =
  Item (Definition (mangleDefinition name) definition) abstractions where
    (definition, abstractions) = runFlattening (unwrap term) name
