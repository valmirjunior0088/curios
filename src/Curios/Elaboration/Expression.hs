module Curios.Elaboration.Expression
  (exTranslate
  )
  where

import Curios.Source (Identifier (..), FunctionTypeBinding (..), FunctionBinding (..), Expression (..))
import Curios.Core (Origin (..), Term (..), trApplyVariable)
import Curios.Elaboration.Miscellaneous (idTranslate, ltTranslate)
import Text.Megaparsec (SourcePos)

trAbstractFunctionTypeBinding :: SourcePos -> FunctionTypeBinding -> Term -> Term
trAbstractFunctionTypeBinding sourcePos (FunctionTypeBinding _ selfName inputName inputTypeExpression) term =
  TrFunctionType (OrSource sourcePos) inputType output where
    inputType =
      exTranslate inputTypeExpression
    output self input =
      let
        step =
          case inputName of
            Just (Identifier _ name) -> trApplyVariable name input term
            Nothing -> term
      in
        case selfName of
          Just (Identifier _ name) -> trApplyVariable name self step
          Nothing -> step

trAbstractFunctionBinding :: SourcePos -> FunctionBinding -> Term -> Term
trAbstractFunctionBinding sourcePos (FunctionBinding _ (Identifier _ name)) term =
  TrFunction (OrSource sourcePos) output where
    output input =
      trApplyVariable name input term

exTranslate :: Expression -> Term
exTranslate expression =
  case expression of
    ExLiteral sourcePos literal ->
      ltTranslate sourcePos literal
    ExIdentifier sourcePos identifier ->
      idTranslate sourcePos identifier
    ExFunctionType sourcePos bindings body ->
      foldr (trAbstractFunctionTypeBinding sourcePos) (exTranslate body) bindings
    ExFunction sourcePos bindings body ->
      foldr (trAbstractFunctionBinding sourcePos) (exTranslate body) bindings
    ExApplication sourcePos function arguments ->
      foldl (TrApplication (OrSource sourcePos)) (exTranslate function) (fmap exTranslate arguments)
    ExParens _ expression' ->
      exTranslate expression'
