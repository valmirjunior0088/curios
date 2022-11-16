module Core.Program
  ( Entry (..)
  , Program (..)
  , check
  )
  where

import Core.Syntax (Type, Term)
import qualified Core.Syntax as Syntax

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import qualified Core.Check as Check

import Error (Origin (..), Error (..))
import Control.Monad (when)
import Data.Foldable (foldlM)
import Text.Megaparsec (SourcePos)

data Entry =
  Declaration SourcePos String Type |
  Definition SourcePos String Term
  deriving (Show)

newtype Program =
  Program [Entry]
  deriving (Show)

declaredNameBeingDeclaredAgain :: SourcePos -> String -> Either Error a
declaredNameBeingDeclaredAgain sourcePos name = Left Error
  { origin = Source sourcePos
  , message = "declared name being declared again: " ++ name
  }

declare :: SourcePos -> String -> Type -> Bindings -> Either Error Bindings
declare sourcePos name tipe globals = do
  when (Bindings.declared name globals)
    (declaredNameBeingDeclaredAgain sourcePos name)

  Check.check globals (Syntax.Type Machine) tipe
  Right (Bindings.declare name tipe globals)

undeclaredNameBeingDefined :: SourcePos -> String -> Either Error a
undeclaredNameBeingDefined sourcePos name = Left Error
  { origin = Source sourcePos
  , message = "undeclared name being defined: " ++ name
  }

define :: SourcePos -> String -> Term -> Bindings -> Either Error Bindings
define sourcePos name term globals = do
  tipe <- case Bindings.declaration name globals of
    Nothing -> undeclaredNameBeingDefined sourcePos name
    Just tipe -> Right tipe

  Check.check globals tipe term
  Right (Bindings.define name term globals)

process :: Bindings -> Entry -> Either Error Bindings
process globals = \case
  Declaration sourcePos name tipe -> declare sourcePos name tipe globals
  Definition sourcePos name term -> define sourcePos name term globals

check :: Program -> Either Error Bindings
check (Program entries) = foldlM process Bindings.empty entries
