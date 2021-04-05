module Curios.Elaboration.Program
  (pgCheck
  )
  where

import Curios.Source (Identifier (..), Program (..))
import Curios.Core (Type, Term, trType)
import Curios.Core.Verification (trCheck)
import Curios.Context (Context (..), cnInsertDeclaration, cnLookupDeclaration, cnInsertDefinition)
import Curios.Context.Initial (cnInitial)
import Curios.Elaboration.Statement (pgDeclarations, pgDefinitions)
import Data.Foldable (foldlM)

import Curios.Error
  (Error
  ,erEeRepeatedlyDeclaredName
  ,erEeUndeclaredName
  ,erEeRepeatedlyDefinedName
  )

cnInsertSourceDeclaration :: Identifier -> Type -> Context -> Either Error Context
cnInsertSourceDeclaration (Identifier sourcePos name) termType context = do
  context' <- case cnInsertDeclaration name termType context of
    Nothing -> Left (erEeRepeatedlyDeclaredName sourcePos name)
    Just value -> Right value
  
  trCheck (cnDeclarations context') (cnDefinitions context') trType termType
  
  Right context'

cnInsertSourceDefinition :: Identifier -> Term -> Context -> Either Error Context
cnInsertSourceDefinition (Identifier sourcePos name) term context = do
  termType <- case cnLookupDeclaration name context of
    Nothing -> Left (erEeUndeclaredName sourcePos name)
    Just value -> Right value
  
  context' <- case cnInsertDefinition name term context of
    Nothing -> Left (erEeRepeatedlyDefinedName sourcePos name)
    Just value -> Right value
  
  trCheck (cnDeclarations context') (cnDefinitions context') termType term
  
  Right context'

pgCheck :: Program -> Either Error Context
pgCheck program = do
  let combine construct context (identifier, term) = construct identifier term context
  step <- foldlM (combine cnInsertSourceDeclaration) cnInitial (pgDeclarations program)

  foldlM (combine cnInsertSourceDefinition) step (pgDefinitions program)
