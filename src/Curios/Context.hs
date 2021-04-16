module Curios.Context
  ( Context (..)
  , ctEmpty
  , ctHandleDeclaration
  , ctHandleDefinition
  )
  where

import Curios.Core (Name, Type, Term, trType)
import Curios.Core.Declarations (Declarations, dcEmpty, dcInsert, dcLookup, dcMember)
import Curios.Core.Definitions (Definitions, dfEmpty, dfInsert, dfMember)
import Curios.Core.Verification (trCheck)
import Curios.Context.Error (Kind (..), Error, throw)
import Text.Megaparsec (SourcePos)
import Control.Monad (when)

data Context =
  Context { ctDeclarations :: Declarations, ctDefinitions :: Definitions }

ctEmpty :: Context
ctEmpty =
  Context { ctDeclarations = dcEmpty, ctDefinitions = dfEmpty }

ctHandleDeclaration :: SourcePos -> Name -> Type -> Context -> Either Error Context
ctHandleDeclaration sourcePos name termType (Context { ctDeclarations, ctDefinitions }) = do
  when (dcMember name ctDeclarations) (throw sourcePos name KnRepeatedlyDeclaredName)

  let declarations = dcInsert name termType ctDeclarations

  case trCheck declarations ctDefinitions trType termType of
    Left coreError -> throw sourcePos name (KnCoreError coreError)
    Right () -> return ()
  
  return (Context { ctDeclarations = declarations, ctDefinitions = ctDefinitions })

ctHandleDefinition :: SourcePos -> Name -> Term -> Context -> Either Error Context
ctHandleDefinition sourcePos name term (Context { ctDeclarations, ctDefinitions }) = do
  when (dfMember name ctDefinitions) (throw sourcePos name KnRepeatedlyDefinedName)

  termType <- case dcLookup name ctDeclarations of
    Nothing -> throw sourcePos name KnUndeclaredName
    Just termType -> return termType
  
  let definitions = dfInsert name term ctDefinitions

  case trCheck ctDeclarations definitions termType term of
    Left coreError -> throw sourcePos name (KnCoreError coreError)
    Right () -> return ()
  
  return (Context { ctDeclarations = ctDeclarations, ctDefinitions = definitions })
