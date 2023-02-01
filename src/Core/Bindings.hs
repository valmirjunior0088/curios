module Core.Bindings
  ( Bindings
  , empty
  , declare
  , declaration
  , declared
  , define
  , definition
  , definitions
  )
  where

import Core.Syntax (Type, Term)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (view, (<>~))

data Bindings = Bindings
  { declarations :: [(String, Type)]
  , definitions :: [(String, Term)]
  }
  deriving (Show, Generic)

empty :: Bindings
empty = Bindings
  { declarations = []
  , definitions = []
  }

declare :: String -> Type -> Bindings -> Bindings
declare name tipe = (the @"declarations") <>~ [(name, tipe)]

declaration :: String -> Bindings -> Maybe Type
declaration name = lookup name . view (the @"declarations")

declared :: String -> Bindings -> Bool
declared name = elem name . map fst . view (the @"declarations")

define :: String -> Term -> Bindings -> Bindings
define name term = (the @"definitions") <>~ [(name, term)]

definition :: String -> Bindings -> Maybe Term
definition name = lookup name . view (the @"definitions")

definitions :: Bindings -> [(String, Term)]
definitions = view (the @"definitions")
