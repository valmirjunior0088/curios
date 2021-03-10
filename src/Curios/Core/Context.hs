module Curios.Core.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
  ,cnInitial
  ,showContext
  )
  where

import Curios.Core.Term (Name, Type, Term, trType, showTerm)
import Curios.Core.Verification (trReduce, trCheck)
import Curios.Core.Declarations (Declarations, dcEmpty, dcInsert, dcLookup)
import Curios.Core.Definitions (Definitions, dfEmpty, dfInsert, dfLookup)
import Curios.Core.Prelude (prelude, prDeclarations, prDefinitions)
import Curios.Error (Error (..), showErrorKind)

data Context =
  Context
    {cnDeclarations :: Declarations
    ,cnDefinitions :: Definitions
    }

cnEmpty :: Context
cnEmpty =
  Context
    {cnDeclarations = dcEmpty
    ,cnDefinitions = dfEmpty
    }

cnInsertDeclaration :: Name -> Type -> Context -> Maybe Context
cnInsertDeclaration name termType context = do
  declarations <- dcInsert name termType (cnDeclarations context)

  Just (context { cnDeclarations = declarations })

cnLookupDeclaration :: Name -> Context -> Maybe Type
cnLookupDeclaration name context =
  dcLookup name (cnDeclarations context)

cnInsertDefinition :: Name -> Term -> Context -> Maybe Context
cnInsertDefinition name term context = do
  definitions <- dfInsert name term (cnDefinitions context)

  Just (context { cnDefinitions = definitions })

cnLookupDefinition :: Name -> Context -> Maybe Term
cnLookupDefinition name context =
  dfLookup name (cnDefinitions context)

cnInsertUnsafeDeclaration :: Name -> Type -> Context -> Context
cnInsertUnsafeDeclaration name termType context =
  let
    context' =
      case cnInsertDeclaration name termType context of
        Nothing -> error ("Prelude error: \"" ++ name ++ "\" is repeatedly declared")
        Just value -> value
  in
    case trCheck (cnDeclarations context') (cnDefinitions context') trType termType of
      Left checkError ->
        error ("Prelude error: In \"" ++ name ++ "\"...\n" ++ showErrorKind (erKind checkError))
      Right () ->
        context'

cnInsertUnsafeDefinition :: Name -> Term -> Context -> Context
cnInsertUnsafeDefinition name term context =
  let
    termType =
      case cnLookupDeclaration name context of
        Nothing -> error ("Prelude error: \"" ++ name ++ "\" is undeclared")
        Just value -> value

    context' =
      case cnInsertDefinition name term context of
        Nothing -> error ("Prelude error: \"" ++ name ++ "\" is repeatedly defined")
        Just value -> value
  in
    case trCheck (cnDeclarations context') (cnDefinitions context') termType term of
      Left checkError ->
        error ("Prelude error: In \"" ++ name ++ "\"..." ++ "\n" ++ showErrorKind (erKind checkError))
      Right () -> 
        context'

cnInitial :: Context
cnInitial =
  let
    combine construct context (name, term) = construct name term context
    step = foldl (combine cnInsertUnsafeDeclaration) cnEmpty (prDeclarations prelude)
  in
    foldl (combine cnInsertUnsafeDefinition) step (prDefinitions prelude)

showContext :: String -> Context -> String
showContext name context =
  "Check succeeded!" ++ "\n" ++
    "\n" ++
    case (cnLookupDeclaration name context, cnLookupDefinition name context) of
      (Just declaration, Just definition) ->
        "Declaration:" ++ "\n" ++
          showTerm declaration ++ "\n" ++
          "\n" ++
          "Definition:" ++ "\n" ++
          showTerm definition ++ "\n" ++
          "\n" ++
          "Evaluation:" ++ "\n" ++ 
          showTerm (trReduce (cnDefinitions context) definition) ++ "\n"
      _ ->
        "An undeclared name was supplied for evaluation." ++ "\n"
