module Curios.Error
  (Error (..)
  )
  where

import Curios.Syntax.Expression (Name (..))
import Curios.Core.Term (Type, Term (..))
import Curios.Core.Environment (Environment (..))

data Error =
  ErUnboundName Name |
  ErRepeatedlyBoundName Name |
  ErRepeatedlyDefinedName Name |
  ErNonInferableTerm Term |
  ErIllTypedTerm Environment Type Term |
  ErInvalidInputType Term |
  ErInvalidOutputType Term |
  ErIllTypedApplication Term |
  ErNonFunctionApplication Term
  deriving (Show)
