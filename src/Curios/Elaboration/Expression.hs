module Curios.Elaboration.Expression
  (exTranslate
  )
  where

import Curios.Source.Types (Identifier (..), FunctionTypeBinding (..), FunctionBinding (..), Expression (..))
import Curios.Core.Term (Origin (..), Term (..), trApplyVariable)
import Curios.Elaboration.Miscellaneous (idTranslate, ltTranslate)
import Text.Megaparsec (SourcePos)

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
