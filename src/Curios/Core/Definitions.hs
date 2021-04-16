module Curios.Core.Definitions
  ( Definitions (..)
  , dfEmpty
  , dfInsert
  , dfLookup
  , dfMember
  )
  where

import Curios.Core (Name, Term)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Definitions =
  Definitions (Map Name Term)

dfEmpty :: Definitions
dfEmpty =
  Definitions (Map.empty)

dfInsert :: Name -> Term -> Definitions -> Definitions
dfInsert name term (Definitions definitions) =
  Definitions (Map.insert name term definitions)

dfLookup :: Name -> Definitions -> Maybe Term
dfLookup name (Definitions definitions) =
  Map.lookup name definitions

dfMember :: Name -> Definitions -> Bool
dfMember name (Definitions definitions) =
  Map.member name definitions
