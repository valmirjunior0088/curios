module Curios.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
  ,cnLookupDefinition
  ,pgCheck
  )
  where

import Curios.Translation (exTranslate)
import Curios.Source.Types (Identifier (..), Program (..), pgDeclarations, pgDefinitions)
import Curios.Core.Term (Origin (..), Name (..), Type, Term (..))
import Curios.Core.Declarations (Declarations (..), dcEmpty, dcInsert, dcLookup)
import Curios.Core.Definitions (Definitions (..), dfEmpty, dfInsert, dfLookup)
import Curios.Core.Verification (trCheck)
import Data.Foldable (foldlM)

import Curios.Error
  (Error (..)
  ,erRepeatedlyDeclaredName
  ,erRepeatedlyDefinedName
  ,erUndeclaredName
  )

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

cnInsertDeclaration :: Identifier -> Type -> Context -> Either Error Context
cnInsertDeclaration (Identifier namePos name) termType context =
  do
    declarations <- case dcInsert (Name name) termType (cnDeclarations context) of
      Nothing -> Left (erRepeatedlyDeclaredName (OrSource namePos) (Name name))
      Just declarations -> Right declarations

    trCheck declarations (cnDefinitions context) (TrType OrMachine) termType
    
    Right (context { cnDeclarations = declarations })

cnLookupDeclaration :: Name -> Context -> Maybe Type
cnLookupDeclaration name context =
  dcLookup name (cnDeclarations context)

cnInsertDefinition :: Identifier -> Term -> Context -> Either Error Context
cnInsertDefinition (Identifier namePos name) term context =
  do
    termType <- case dcLookup (Name name) (cnDeclarations context) of
      Nothing -> Left (erUndeclaredName (OrSource namePos) (Name name))
      Just termType -> Right termType

    definitions <- case dfInsert (Name name) term (cnDefinitions context) of
      Nothing -> Left (erRepeatedlyDefinedName (OrSource namePos) (Name name))
      Just definitions -> Right definitions

    trCheck (cnDeclarations context) definitions termType term

    Right (context { cnDefinitions = definitions })

cnLookupDefinition :: Name -> Context -> Maybe Term
cnLookupDefinition name context =
  dfLookup name (cnDefinitions context)

pgCheck :: Program -> Either Error Context
pgCheck program =
  do
    context <- build cnInsertDeclaration (pgDeclarations program) cnEmpty
    
    build cnInsertDefinition (pgDefinitions program) context
  where
    combine insert context (name, expression) =
      insert name (exTranslate expression) context
    build insert expressions context =
      foldlM (combine insert) context expressions
