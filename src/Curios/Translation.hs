module Curios.Translation
  (trSubstitute
  ,trAbstractFunctionTypeVariable
  ,trAbstractFunctionVariable
  ,exTranslate
  ,pgTranslate
  )
  where

import Data.Foldable
  (foldlM
  )

import Curios.Error
  (Error (..)
  )

import Curios.Syntax.Expression
  (Expression (..)
  ,Name (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Program (..)
  ,pgBindings
  ,pgDefinitions
  )

import Curios.Core.Term
  (Term (..)
  ,PrimitiveType (..)
  )

import Curios.Core.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertBinding
  ,cnInsertDefinition
  )

trSubstitute :: Name -> Term -> Term -> Term
trSubstitute name source term =
  case term of
    TrReference name' | name == name' ->
      source
    TrFunctionType input output ->
      TrFunctionType
        (trSubstitute name source input)
        (\self variable -> trSubstitute name source (output self variable))
    TrFunction output ->
      TrFunction (\variable -> trSubstitute name source (output variable))
    TrApplication function argument ->
      TrApplication
        (trSubstitute name source function)
        (trSubstitute name source argument)
    term' ->
      term'

trAbstractFunctionTypeVariable :: Maybe Name -> FunctionTypeVariable -> Term -> Term
trAbstractFunctionTypeVariable selfName (FunctionTypeVariable variableName expression') term =
  TrFunctionType input output where
    input = exTranslate expression'
    output self variable =
      let
        term' = case selfName of
          Nothing -> term
          Just selfName' -> trSubstitute selfName' self term
      in
        case variableName of
          Nothing -> term'
          Just variableName' -> trSubstitute variableName' variable term'

trAbstractFunctionVariable :: FunctionVariable -> Term -> Term
trAbstractFunctionVariable (FunctionVariable variableName) term =
  TrFunction output where
    output variable = trSubstitute variableName variable term

exTranslate :: Expression -> Term
exTranslate expression =
  case expression of
    ExName (Name "Type") -> TrType
    ExName (Name "Text") -> TrPrimitiveType PtText
    ExName (Name "Integer") -> TrPrimitiveType PtInteger
    ExName (Name "Rational") -> TrPrimitiveType PtRational
    ExName name -> TrReference name
    ExPrimitive primitive -> TrPrimitive primitive
    ExFunctionType selfName variables body ->
      case variables of
        [] ->
          exTranslate body
        (variable : variables') ->
          trAbstractFunctionTypeVariable selfName variable
            (build (trAbstractFunctionTypeVariable Nothing) variables' body)
    ExFunction variables body ->
      build trAbstractFunctionVariable variables body
    ExApplication function arguments ->
      foldl TrApplication (exTranslate function) (fmap exTranslate arguments)
  where
    build abstract variables body =
      foldr abstract (exTranslate body) variables
      
pgTranslate :: Program -> Either Error Context
pgTranslate program =
  do
    context <- build cnInsertBinding (pgBindings program) cnEmpty
    build cnInsertDefinition (pgDefinitions program) context
  where
    combine insert context (name, expression) =
      insert name (exTranslate expression) context
    build insert expressions context =
      foldlM (combine insert) context expressions
      