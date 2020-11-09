module Curios.Core.Bindings
  (Bindings (..)
  ,bnEmpty
  ,bnInsert
  ,bnLookup
  )
  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Curios.Error (Error (..))
import Curios.Syntax.Expression (Name (..))
import Curios.Core.Term (Type)

newtype Bindings =
  Bindings (Map Name Type)

bnEmpty :: Bindings
bnEmpty =
  Bindings (Map.empty)

bnInsert :: Name -> Type -> Bindings -> Either Error Bindings
bnInsert name termType (Bindings bindings) =
  case Map.member name bindings of
    True -> Left (ErRepeatedlyBoundName name)
    False -> Right (Bindings (Map.insert name termType bindings))

bnLookup :: Name -> Bindings -> Either Error Type
bnLookup name (Bindings bindings) =
  case Map.lookup name bindings of
    Nothing -> Left (ErUnboundName name)
    Just termType -> Right termType
