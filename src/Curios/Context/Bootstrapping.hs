module Curios.Context.Bootstrapping
  (ctFromProgram
  )
  where

import Curios.Source (Program)
import Curios.Context (Context, ctHandleDeclaration, ctHandleDefinition)
import Curios.Context.Initial (ctInitial)
import Curios.Context.Error (Error)
import Curios.Elaboration.Program (pgDeclarations, pgDefinitions)
import Data.Foldable (foldlM)

ctFromProgram :: Program -> Either Error Context
ctFromProgram program = do
  let declare context (sourcePos, name, termType) = ctHandleDeclaration sourcePos name termType context
  step <- foldlM declare ctInitial (pgDeclarations program)

  let define context (sourcePos, name, term) = ctHandleDefinition sourcePos name term context
  foldlM define step (pgDefinitions program)
