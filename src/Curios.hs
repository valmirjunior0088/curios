module Curios
  (run
  )
  where

import Curios.Core (Name, trShow)
import Curios.Core.Declarations (dcLookup)
import Curios.Core.Definitions (dfLookup)
import Curios.Core.Reduction (trReduce)
import Curios.Context (Context (..))
import Curios.Elaboration.Execution (pgCheck)

import qualified Curios.Source.Parser as Source
import qualified Curios.Source.Error as Source
import qualified Curios.Elaboration.Error as Elaboration

evaluate :: Context -> Name -> String
evaluate (Context { ctDeclarations, ctDefinitions }) name =
  case (dcLookup name ctDeclarations, dfLookup name ctDefinitions) of
    (Just declaration, Just definition) ->
      "Declaration:" ++ "\n"
        ++ trShow 0 declaration ++ "\n"
        ++ "\n"
        ++ "Definition:" ++ "\n"
        ++ trShow 0 definition ++ "\n"
        ++ "\n"
        ++ "Evaluation:" ++ "\n"
        ++ trShow 0 (trReduce ctDefinitions definition) ++ "\n"
    _ ->
      "An undeclared name was supplied for evaluation" ++ "\n"

check :: String -> Maybe String -> String -> Either String String
check file name source = do
  program <- case Source.parse file source of
    Left sourceError -> Left (Source.showError source sourceError)
    Right program -> Right program

  context <- case pgCheck program of
    Left elaborationError -> Left (Elaboration.showError source elaborationError)
    Right context -> Right context
  
  Right (maybe "Check succeded!\n" (evaluate context) name)

run :: String -> Maybe String -> String -> IO ()
run file name =
  putStr . either id id . check file name
