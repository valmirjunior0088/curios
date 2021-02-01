module Curios.Core.Declarations
  (Declarations (..)
  ,dcEmpty
  ,dcInsert
  ,dcLookup
  )
  where

import Curios.Core.Term (Name (..), Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Declarations =
  Declarations (Map Name Type)

dcEmpty :: Declarations
dcEmpty =
  Declarations (Map.empty)

dcInsert :: Name -> Type -> Declarations -> Maybe Declarations
dcInsert name termType (Declarations declarations) =
  if Map.member name declarations
    then Nothing
    else Just (Declarations (Map.insert name termType declarations))

dcLookup :: Name -> Declarations -> Maybe Type
dcLookup name (Declarations declarations) =
  Map.lookup name declarations
