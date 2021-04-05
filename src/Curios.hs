module Curios
  (check
  ,evaluate
  )
  where

import Prelude hiding (error)

import Curios.Core.Term (Name)
import Curios.Core.Context (Context (..), cnLookupDeclaration, cnLookupDefinition)
import Curios.Core.Verification (trReduce)
import Curios.Elaboration.Program (pgCheck)
import Curios.Source.Parser (parse)
import Curios.Error (Error)

check :: String -> String -> Either Error Context
check file source =
  parse file source >>= pgCheck

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
