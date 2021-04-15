module Curios.Elaboration
  (ctHandleDeclaration
  ,ctHandleDefinition
  )
  where

import Curios.Core (Name, Type, Term, trType)
import Curios.Core.Declarations (dcInsert, dcLookup)
import Curios.Core.Definitions (dfInsert)
import Curios.Core.Verification (trCheck)
import Curios.Context (Context (..))
import Curios.Elaboration.Error (Kind (..), Error, throw)
import Text.Megaparsec (SourcePos)

ctHandleDeclaration :: SourcePos -> Name -> Type -> Context -> Either Error Context
ctHandleDeclaration sourcePos name termType (Context { ctDeclarations, ctDefinitions }) = do
  let declarations = dcInsert name termType ctDeclarations

  case trCheck declarations ctDefinitions trType termType of
    Left coreError -> throw sourcePos name (KnCoreError coreError)
    Right () -> return ()
  
  return (Context { ctDeclarations = declarations, ctDefinitions = ctDefinitions })

ctHandleDefinition :: SourcePos -> Name -> Term -> Context -> Either Error Context
ctHandleDefinition sourcePos name term (Context { ctDeclarations, ctDefinitions }) = do
  termType <- case dcLookup name ctDeclarations of
    Nothing -> throw sourcePos name KnUndeclaredName
    Just termType -> return termType
  
  let definitions = dfInsert name term ctDefinitions

  case trCheck ctDeclarations definitions termType term of
    Left coreError -> throw sourcePos name (KnCoreError coreError)
    Right () -> return ()
  
  return (Context { ctDeclarations = ctDeclarations, ctDefinitions = definitions })
