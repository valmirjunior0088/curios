module Curios.Translation
  (pgCheck
  )
  where

import Curios.Core.Verification (trCheck)
import Text.Megaparsec (SourcePos (..))
import Data.Foldable (foldlM)
import qualified Curios.Source.Types as Source (Literal (..))
import qualified Curios.Core.Term as Core (Literal (..))

import Curios.Source.Types
  (Identifier (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Binding (..)
  ,Expression (..)
  ,Prefix (..)
  ,Statement (..)
  ,Program (..)
  )

import Curios.Core.Term
  (Origin (..)
  ,Primitive (..)
  ,Name
  ,Type
  ,Argument (..)
  ,Term (..)
  ,trAbstract
  ,trSubstitute
  ,trType
  )

import Curios.Core.Context
  (Context (..)
  ,cnInsertDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
  ,cnInitial
  )

import Curios.Error
  (Error (..)
  ,erRepeatedlyDeclaredName
  ,erRepeatedlyDefinedName
  ,erUndeclaredName
  )

idTranslate :: SourcePos -> Identifier -> Term
idTranslate sourcePos identifier =
  case identifier of
    Identifier _ "Type" -> TrType (OrSource sourcePos)
    Identifier _ "Text" -> TrPrimitive (OrSource sourcePos) PrText
    Identifier _ "Integer" -> TrPrimitive (OrSource sourcePos) PrInteger
    Identifier _ "Real" -> TrPrimitive (OrSource sourcePos) PrReal
    Identifier _ name -> TrReference (OrSource sourcePos) name

ltTranslate :: SourcePos -> Source.Literal -> Term
ltTranslate sourcePos literal =
  case literal of
    Source.LtText _ string -> TrLiteral (OrSource sourcePos) (Core.LtText string)
    Source.LtInteger _ integer -> TrLiteral (OrSource sourcePos) (Core.LtInteger integer)
    Source.LtReal _ double -> TrLiteral (OrSource sourcePos) (Core.LtReal double)

trApplyArgument :: Name -> Argument -> Term -> Term
trApplyArgument name argument term =
  case argument of
    ArPlaceholder depth -> trAbstract name depth term
    ArTerm source -> trSubstitute name source term

trAbstractFunctionTypeVariable :: SourcePos -> FunctionTypeVariable -> Term -> Term
trAbstractFunctionTypeVariable functionTypePos (FunctionTypeVariable _ variableName expression) term =
  TrFunctionType (OrSource functionTypePos) input output where
    input =
      exTranslate expression
    output _ variableArgument =
      case variableName of
        Just (Identifier _ name) ->
          trApplyArgument name variableArgument term
        Nothing ->
          term

trAbstractFunctionType :: SourcePos -> Maybe Identifier -> [FunctionTypeVariable] -> Term -> Term
trAbstractFunctionType sourcePos selfName variables term =
  case foldr (trAbstractFunctionTypeVariable sourcePos) term variables of
    TrFunctionType origin input output ->
      TrFunctionType origin input output' where
        output' selfArgument variableArgument =
          case selfName of
            Just (Identifier _ name) ->
              trApplyArgument name selfArgument (output selfArgument variableArgument)
            Nothing ->
              output selfArgument variableArgument
    term ->
      term

trAbstractFunctionVariable :: SourcePos -> FunctionVariable -> Term -> Term
trAbstractFunctionVariable functionPos (FunctionVariable _ (Identifier _ name)) term =
  TrFunction (OrSource functionPos) output where
    output variableArgument =
      trApplyArgument name variableArgument term

exTranslate :: Expression -> Term
exTranslate expression =
  case expression of
    ExLiteral sourcePos literal ->
      ltTranslate sourcePos literal
    ExIdentifier sourcePos identifier ->
      idTranslate sourcePos identifier
    ExFunctionType sourcePos selfName variables body ->
      trAbstractFunctionType sourcePos selfName variables (exTranslate body)
    ExFunction sourcePos variables body ->
      foldr (trAbstractFunctionVariable sourcePos) (exTranslate body) variables
    ExApplication sourcePos function arguments ->
      foldl (TrApplication (OrSource sourcePos)) (exTranslate function) (fmap exTranslate arguments)

trAbstractDeclarationBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDeclarationBinding sourcePos (Binding _ (Identifier _ name) expression) term =
  TrFunctionType (OrSource sourcePos) (exTranslate expression) output where
    output _ variableArgument = trApplyArgument name variableArgument term

trAbstractDefinitionBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDefinitionBinding sourcePos (Binding _ (Identifier _ name) _) term =
  TrFunction (OrSource sourcePos) output where
    output argument = trApplyArgument name argument term

pgDeclarations :: Program -> [(Identifier, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos variables) output _) =
      (identifier, foldr (trAbstractDeclarationBinding sourcePos) (exTranslate output) variables)

pgDefinitions :: Program -> [(Identifier, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos variables) _ expression) =
      (identifier, foldr (trAbstractDefinitionBinding sourcePos) (exTranslate expression) variables)

cnInsertSourceDeclaration :: Identifier -> Type -> Context -> Either Error Context
cnInsertSourceDeclaration (Identifier sourcePos name) termType context = do
  context' <- case cnInsertDeclaration name termType context of
    Nothing -> Left (erRepeatedlyDeclaredName (OrSource sourcePos) name)
    Just value -> Right value
  
  trCheck (cnDeclarations context') (cnDefinitions context') trType termType
  
  Right context'

cnInsertSourceDefinition :: Identifier -> Term -> Context -> Either Error Context
cnInsertSourceDefinition (Identifier sourcePos name) term context = do
  termType <- case cnLookupDeclaration name context of
    Nothing -> Left (erUndeclaredName (OrSource sourcePos) name)
    Just value -> Right value
  
  context' <- case cnInsertDefinition name term context of
    Nothing -> Left (erRepeatedlyDefinedName (OrSource sourcePos) name)
    Just value -> Right value
  
  trCheck (cnDeclarations context') (cnDefinitions context') termType term
  
  Right context'

pgCheck :: Program -> Either Error Context
pgCheck program = do
  let combine construct context (identifier, term) = construct identifier term context
  step <- foldlM (combine cnInsertSourceDeclaration) cnInitial (pgDeclarations program)

  foldlM (combine cnInsertSourceDefinition) step (pgDefinitions program)
