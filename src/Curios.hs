module Curios
  (check
  ,evaluate
  )
  where

import Curios.Source.Parser (parse)
import Curios.Core (Name)
import Curios.Core.Verification (trReduce)
import Curios.Context (Context (..), cnLookupDeclaration, cnLookupDefinition)
import Curios.Elaboration.Program (pgCheck)
import Curios.Error (Error (..))

check :: String -> String -> Either Error Context
check file source = do
  program <- case parse file source of
    Left sourceError -> Left (ErSource sourceError)
    Right program -> Right program
  
  case pgCheck program of
    Left elaborationError -> Left (ErElaboration elaborationError)
    Right context -> Right context

evaluate :: Name -> Context -> String
evaluate name context =
  "Check succeeded!" ++ "\n"
    ++ "\n"
    ++ case (cnLookupDeclaration name context, cnLookupDefinition name context) of
      (Just declaration, Just definition) ->
        "Declaration:" ++ "\n"
          ++ show declaration ++ "\n"
          ++ "\n"
          ++ "Definition:" ++ "\n"
          ++ show definition ++ "\n"
          ++ "\n"
          ++ "Evaluation:" ++ "\n"
          ++ show (trReduce (cnDefinitions context) definition) ++ "\n"
      _ ->
        "An undeclared name was supplied for evaluation." ++ "\n"
