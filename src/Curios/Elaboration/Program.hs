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
import Curios.Elaboration.Error (Error (..))
import Data.Foldable (foldlM)

cnInsertSourceDeclaration :: Identifier -> Type -> Context -> Either Error Context
cnInsertSourceDeclaration (Identifier sourcePos name) termType context = do
  context' <- case cnInsertDeclaration name termType context of
    Nothing -> Left (ErRepeatedlyDeclaredName sourcePos name)
    Just context' -> Right context'
  
  case trCheck (cnDeclarations context') (cnDefinitions context') trType termType of
    Left coreError -> Left (ErCoreError coreError name)
    Right () -> Right context'

cnInsertSourceDefinition :: Identifier -> Term -> Context -> Either Error Context
cnInsertSourceDefinition (Identifier sourcePos name) term context = do
  termType <- case cnLookupDeclaration name context of
    Nothing -> Left (ErUndeclaredName sourcePos name)
    Just termType -> Right termType
  
  context' <- case cnInsertDefinition name term context of
    Nothing -> Left (ErRepeatedlyDefinedName sourcePos name)
    Just context' -> Right context'
  
  case trCheck (cnDeclarations context') (cnDefinitions context') termType term of
    Left coreError -> Left (ErCoreError coreError name)
    Right () -> Right context'

pgCheck :: Program -> Either Error Context
pgCheck program = do
  let combine construct context (identifier, term) = construct identifier term context
  step <- foldlM (combine cnInsertSourceDeclaration) cnInitial (pgDeclarations program)

  foldlM (combine cnInsertSourceDefinition) step (pgDefinitions program)
