module Curios.Core.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertBinding
  ,cnLookupBinding
  ,cnInsertDefinition
  ,cnLookupDefinition
  )
  where

import Curios.Error (Error (..))
import Curios.Syntax.Expression (Name (..))
import Curios.Core.Term (Type, Term (..))
import Curios.Core.Bindings (Bindings (..), bnEmpty, bnInsert, bnLookup)
import Curios.Core.Definitions (Definitions (..), dfEmpty, dfInsert, dfLookup)
import Curios.Core.Verification (trCheck)

data Context =
  Context
    {cnBindings :: Bindings
    ,cnDefinitions :: Definitions
    }

cnEmpty :: Context
cnEmpty =
  Context
    {cnBindings = bnEmpty
    ,cnDefinitions = dfEmpty
    }

cnInsertBinding :: Name -> Type -> Context -> Either Error Context
cnInsertBinding name termType context =
  do
    bindings <- bnInsert name termType (cnBindings context)
    let context' = context { cnBindings = bindings }

    trCheck (cnBindings context') (cnDefinitions context') TrType termType
    
    Right context'

cnLookupBinding :: Name -> Context -> Either Error Type
cnLookupBinding name context =
  bnLookup name (cnBindings context)

cnInsertDefinition :: Name -> Term -> Context -> Either Error Context
cnInsertDefinition name term context =
  do
    definitions <- dfInsert name term (cnDefinitions context)
    let context' = context { cnDefinitions = definitions }

    termType <- bnLookup name (cnBindings context')
    trCheck (cnBindings context') (cnDefinitions context') termType term

    Right context'

cnLookupDefinition :: Name -> Context -> Maybe Term
cnLookupDefinition name context =
  dfLookup name (cnDefinitions context)
