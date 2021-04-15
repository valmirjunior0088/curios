module Curios.Context
  (Context (..)
  ,ctEmpty
  )
  where

import Curios.Core.Declarations (Declarations, dcEmpty)
import Curios.Core.Definitions (Definitions, dfEmpty)

data Context =
  Context { ctDeclarations :: Declarations, ctDefinitions :: Definitions }

ctEmpty :: Context
ctEmpty =
  Context { ctDeclarations = dcEmpty, ctDefinitions = dfEmpty }
