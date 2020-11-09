module Curios.Core.Definitions
  (Definitions (..)
  ,dfEmpty
  ,dfInsert
  ,dfLookup
  )
  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Curios.Error (Error (..))
import Curios.Syntax.Expression (Name (..))
import Curios.Core.Term (Term (..))

newtype Definitions =
  Definitions (Map Name Term)

dfEmpty :: Definitions
dfEmpty =
  Definitions (Map.empty)

dfInsert :: Name -> Term -> Definitions -> Either Error Definitions
dfInsert name term (Definitions definitions) =
  case Map.member name definitions of
    True -> Left (ErRepeatedlyDefinedName name)
    False -> Right (Definitions (Map.insert name term definitions))

dfLookup :: Name -> Definitions -> Maybe Term
dfLookup name (Definitions definitions) =
  Map.lookup name definitions
