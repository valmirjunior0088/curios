module Curios.Translation
  (pgCheck
  )
  where

import Curios.Core.Verification (trCheck)
import Curios.Core.Prelude (cnInitial)
import Text.Megaparsec (SourcePos (..))
import Data.Foldable (foldlM)
import qualified Curios.Source.Types as Source (Literal (..))
import qualified Curios.Core.Term as Core (Literal (..))

import Curios.Source.Types
  (Identifier (..)
  ,FunctionTypeBinding (..)
  ,FunctionBinding (..)
  ,Binding (..)
  ,Expression (..)
  ,Prefix (..)
  ,Statement (..)
  ,Program (..)
  )

import Curios.Core.Term
  (Origin (..)
  ,Primitive (..)
  ,Type
  ,Term (..)
  ,trType
  ,trApplyVariable
  )

import Curios.Core.Context
  (Context (..)
  ,cnInsertDeclaration
  ,cnLookupDeclaration
  ,cnInsertDefinition
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

trAbstractFunctionTypeBinding :: SourcePos -> FunctionTypeBinding -> Term -> Term
trAbstractFunctionTypeBinding functionTypePos (FunctionTypeBinding _ inputName inputTypeExpression) term =
  TrFunctionType (OrSource functionTypePos) inputType output where
    inputType =
      exTranslate inputTypeExpression
    output _ input =
      case inputName of
        Just (Identifier _ name) ->
          trApplyVariable name input term
        Nothing ->
          term

trAbstractFunctionType :: SourcePos -> Maybe Identifier -> [FunctionTypeBinding] -> Term -> Term
trAbstractFunctionType sourcePos selfName bindings term =
  case foldr (trAbstractFunctionTypeBinding sourcePos) term bindings of
    TrFunctionType origin inputType output ->
      TrFunctionType origin inputType output' where
        output' self input =
          case selfName of
            Just (Identifier _ name) ->
              trApplyVariable name self (output self input)
            Nothing ->
              output self input
    term' ->
      term'

trAbstractFunctionBinding :: SourcePos -> FunctionBinding -> Term -> Term
trAbstractFunctionBinding functionPos (FunctionBinding _ (Identifier _ name)) term =
  TrFunction (OrSource functionPos) output where
    output input =
      trApplyVariable name input term

exTranslate :: Expression -> Term
exTranslate expression =
  case expression of
    ExLiteral sourcePos literal ->
      ltTranslate sourcePos literal
    ExIdentifier sourcePos identifier ->
      idTranslate sourcePos identifier
    ExFunctionType sourcePos selfName bindings body ->
      trAbstractFunctionType sourcePos selfName bindings (exTranslate body)
    ExFunction sourcePos bindings body ->
      foldr (trAbstractFunctionBinding sourcePos) (exTranslate body) bindings
    ExApplication sourcePos function arguments ->
      foldl (TrApplication (OrSource sourcePos)) (exTranslate function) (fmap exTranslate arguments)

trAbstractDeclarationBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDeclarationBinding sourcePos (Binding _ (Identifier _ name) expression) term =
  TrFunctionType (OrSource sourcePos) (exTranslate expression) output where
    output _ input = trApplyVariable name input term

trAbstractDefinitionBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDefinitionBinding sourcePos (Binding _ (Identifier _ name) _) term =
  TrFunction (OrSource sourcePos) output where
    output input = trApplyVariable name input term

pgDeclarations :: Program -> [(Identifier, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos bindings) declaration _) =
      (identifier, foldr (trAbstractDeclarationBinding sourcePos) (exTranslate declaration) bindings)

pgDefinitions :: Program -> [(Identifier, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos bindings) _ definition) =
      (identifier, foldr (trAbstractDefinitionBinding sourcePos) (exTranslate definition) bindings)

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
