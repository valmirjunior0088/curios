module Curios.Core.Declarations
  (Declarations (..)
  ,dcEmpty
  ,dcInsert
  ,dcLookup
  ,dcMember
  )
  where

import Curios.Core (Name, Type)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Declarations =
  Declarations (Map Name Type)

dcEmpty :: Declarations
dcEmpty =
  Declarations (Map.empty)

dcInsert :: Name -> Type -> Declarations -> Declarations
dcInsert name termType (Declarations declarations) =
  Declarations (Map.insert name termType declarations)

dcLookup :: Name -> Declarations -> Maybe Type
dcLookup name (Declarations declarations) =
  Map.lookup name declarations

dcMember :: Name -> Declarations -> Bool
dcMember name (Declarations declarations) =
  Map.member name declarations
