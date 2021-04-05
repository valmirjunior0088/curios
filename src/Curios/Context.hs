module Curios.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
  ,cnLookupDefinition
  )
  where

import Curios.Core (Name, Type, Term)
import Curios.Core.Declarations (Declarations, dcEmpty, dcInsert, dcLookup)
import Curios.Core.Definitions (Definitions, dfEmpty, dfInsert, dfLookup)

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
