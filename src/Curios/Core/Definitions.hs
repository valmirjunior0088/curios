module Curios.Core.Definitions
  (Definitions (..)
  ,dfEmpty
  ,dfInsert
  ,dfLookup
  )
  where

import Curios.Core.Term (Name (..), Term (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Definitions =
  Definitions (Map Name Term)

dfEmpty :: Definitions
dfEmpty =
  Definitions (Map.empty)

dfInsert :: Name -> Term -> Definitions -> Maybe Definitions
dfInsert name term (Definitions definitions) =
  if Map.member name definitions
    then Nothing
    else Just (Definitions (Map.insert name term definitions))

dfLookup :: Name -> Definitions -> Maybe Term
dfLookup name (Definitions definitions) =
  Map.lookup name definitions
