module Curios.Context.Initial
  (ctInitial
  )
  where

import Curios.Context (Context, ctEmpty)
import Curios.Elaboration (ctHandleDeclaration, ctHandleDefinition)
import Curios.Elaboration.Error (showError)
import Text.Megaparsec (SourcePos (..), mkPos)
import Data.Foldable (foldlM)

import qualified Curios.Context.Prelude as Prelude

sourcePos :: SourcePos
sourcePos =
  SourcePos { sourceName = "<prelude>", sourceLine = mkPos 1, sourceColumn = mkPos 1 }

ctInitial :: Context
ctInitial =
  case result of
    Left elaborationError -> error ("\n" ++ showError "<prelude>" elaborationError)
    Right context -> context
  where
    result = do
      let declare context (name, termType) = ctHandleDeclaration sourcePos name termType context
      step <- foldlM declare ctEmpty Prelude.declarations
      
      let define context (name, term) = ctHandleDefinition sourcePos name term context
      foldlM define step Prelude.definitions
