module Curios.Context.Initial
  (cnInitial
  )
  where

import Curios.Core (Name, Type, Term, trType)
import Curios.Core.Verification (trCheck)
import Curios.Context (Context (..), cnEmpty, cnInsertDeclaration, cnLookupDeclaration, cnInsertDefinition)
import Curios.Context.Prelude (prelude, prDeclarations, prDefinitions)
import Curios.Error (showError)

cnInsertUnsafeDeclaration :: Name -> Type -> Context -> Context
cnInsertUnsafeDeclaration name termType context =
  let
    context' =
      case cnInsertDeclaration name termType context of
        Nothing -> error ("Prelude error: \"" ++ name ++ "\" is repeatedly declared")
        Just value -> value
  in
    case trCheck (cnDeclarations context') (cnDefinitions context') trType termType of
      Left curiosError ->
        error ("Prelude error: In \"" ++ name ++ "\"...\n" ++ showError curiosError "<prelude>")
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
      Left curiosError ->
        error ("Prelude error: In \"" ++ name ++ "\"..." ++ "\n" ++ showError curiosError "<prelude>")
      Right () -> 
        context'

cnInitial :: Context
cnInitial =
  let
    combine construct context (name, term) = construct name term context
    step = foldl (combine cnInsertUnsafeDeclaration) cnEmpty (prDeclarations prelude)
  in
    foldl (combine cnInsertUnsafeDefinition) step (prDefinitions prelude)
