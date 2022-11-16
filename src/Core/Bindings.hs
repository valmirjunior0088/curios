module Core.Bindings
  ( Bindings (..)
  , empty
  , declare
  , declaration
  , declarations
  , declared
  , define
  , definition
  , definitions
  )
  where

import Core.Syntax (Type, Term)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (view, over)

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
declare name tipe = over (the @"declarations") (++ [(name, tipe)])

declaration :: String -> Bindings -> Maybe Type
declaration name = lookup name . view (the @"declarations")

declarations :: Bindings -> [String]
declarations bindings = [name | (name, _) <- view (the @"declarations") bindings]

declared :: String -> Bindings -> Bool
declared name bindings = name `elem ` declarations bindings

define :: String -> Term -> Bindings -> Bindings
define name term bindings =
  if not (declared name bindings)
  then errorWithoutStackTrace ("Bindings.define: undeclared name \"" ++ name ++ "\"")
  else over (the @"definitions") (++ [(name, term)]) bindings

definition :: String -> Bindings -> Maybe Term
definition name = lookup name . view (the @"definitions")

definitions :: Bindings -> [(String, Term)]
definitions = view (the @"definitions")