module Curios.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertDeclaration
  ,cnInsertSourceDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
  ,cnInsertSourceDefinition
  ,cnLookupDefinition
  ,pgCheck
  ,showContext
  )
  where

import Curios.Translation (exTranslate, trAbstractDeclarationBinding, trAbstractDefinitionBinding)
import Curios.Source.Types (Identifier (..), Program (..), Prefix (..), Statement (..))
import Curios.Core.Term (Origin (..), Name, Type, Term (..), trType, showTerm)
import Curios.Core.Declarations (Declarations (..), dcEmpty, dcInsert, dcLookup)
import Curios.Core.Definitions (Definitions (..), dfEmpty, dfInsert, dfLookup)
import Curios.Core.Verification (trReduce, trCheck)
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)

import Curios.Error
  (Error (..)
  ,erRepeatedlyDeclaredName
  ,erRepeatedlyDefinedName
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

cnInsertDeclaration :: Origin -> Name -> Type -> Context -> Either Error Context
cnInsertDeclaration origin name termType context = do
  declarations <- case dcInsert name termType (cnDeclarations context) of
    Nothing -> Left (erRepeatedlyDeclaredName origin name)
    Just declarations -> Right declarations

  trCheck declarations (cnDefinitions context) trType termType
  
  Right (context { cnDeclarations = declarations })

cnInsertSourceDeclaration :: Identifier -> Type -> Context -> Either Error Context
cnInsertSourceDeclaration (Identifier namePos name) termType context =
  cnInsertDeclaration (OrSource namePos) name termType context

cnLookupDeclaration :: Name -> Context -> Maybe Type
cnLookupDeclaration name context =
  dcLookup name (cnDeclarations context)

cnInsertDefinition :: Origin -> Name -> Term -> Context -> Either Error Context
cnInsertDefinition origin name term context = do
  let termType = fromJust (dcLookup name (cnDeclarations context))

  definitions <- case dfInsert name term (cnDefinitions context) of
    Nothing -> Left (erRepeatedlyDefinedName origin name)
    Just definitions -> Right definitions

  trCheck (cnDeclarations context) definitions termType term

  Right (context { cnDefinitions = definitions })

cnInsertSourceDefinition :: Identifier -> Term -> Context -> Either Error Context
cnInsertSourceDefinition (Identifier namePos name) term context =
  cnInsertDefinition (OrSource namePos) name term context

cnLookupDefinition :: Name -> Context -> Maybe Term
cnLookupDefinition name context =
  dfLookup name (cnDefinitions context)

pgDeclarations :: Program -> [(Identifier, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (StLet _ identifier (Prefix sourcePos variables) output _) =
      (identifier, foldr (trAbstractDeclarationBinding sourcePos) (exTranslate output) variables)

pgDefinitions :: Program -> [(Identifier, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (StLet _ identifier (Prefix sourcePos variables) _ expression) =
      (identifier, foldr (trAbstractDefinitionBinding sourcePos) (exTranslate expression) variables)

pgCheck :: Context -> Program -> Either Error Context
pgCheck programContext program =
  do
    programContext' <- foldlM combineDeclaration programContext (pgDeclarations program)
    foldlM combineDefinitions programContext' (pgDefinitions program)
  where
    combineDeclaration context (identifier, term) =
      cnInsertSourceDeclaration identifier term context
    combineDefinitions context (identifier, term) =
      cnInsertSourceDefinition identifier term context

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
