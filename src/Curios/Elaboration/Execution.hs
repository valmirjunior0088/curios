module Curios.Elaboration.Execution
  (pgCheck
  )
  where

import Curios.Source (Program)
import Curios.Context (Context)
import Curios.Context.Initial (ctInitial)
import Curios.Elaboration (ctHandleDeclaration, ctHandleDefinition)
import Curios.Elaboration.Program (pgDeclarations, pgDefinitions)
import Curios.Elaboration.Error (Error)
import Data.Foldable (foldlM)

pgCheck :: Program -> Either Error Context
pgCheck program = do
  let declare context (sourcePos, name, termType) = ctHandleDeclaration sourcePos name termType context
  step <- foldlM declare ctInitial (pgDeclarations program)

  let define context (sourcePos, name, term) = ctHandleDefinition sourcePos name term context
  foldlM define step (pgDefinitions program)
