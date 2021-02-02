module Curios.Translation
  (exTranslate
  )
  where

import qualified Curios.Source.Types as Source (Literal (..))
import qualified Curios.Core.Term as Core (Literal (..))
import Text.Megaparsec (SourcePos (..))

import Curios.Source.Types
  (Identifier (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Expression (..)
  )

import Curios.Core.Term
  (Origin (..)
  ,Primitive (..)
  ,Name
  ,Argument (..)
  ,Term (..)
  ,trAbstract
  ,trSubstitute
  )

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

trAbstractFunctionVariable :: SourcePos -> FunctionVariable -> Term -> Term
trAbstractFunctionVariable functionPos (FunctionVariable _ (Identifier _ name)) term =
  TrFunction (OrSource functionPos) output where
    output variableArgument =
      trApplyArgument name variableArgument term

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

exTranslate :: Expression -> Term
exTranslate expression =
  case expression of
    ExIdentifier sourcePos identifier ->
      idTranslate sourcePos identifier
    ExLiteral sourcePos literal ->
      ltTranslate sourcePos literal
    ExFunctionType sourcePos selfName variables body ->
      case foldr (trAbstractFunctionTypeVariable sourcePos) (exTranslate body) variables of
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
    ExFunction sourcePos variables body ->
      foldr (trAbstractFunctionVariable sourcePos) (exTranslate body) variables
    ExApplication sourcePos function arguments ->
      foldl (TrApplication (OrSource sourcePos)) (exTranslate function) (fmap exTranslate arguments)
