module Curios.Translation
  (trSubstitute
  ,trDischargeDependentVariable
  ,trDischargeVariable
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
  ,DependentVariable (..)
  ,Variable (..)
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

trDischargeDependentVariable :: DependentVariable -> Term -> Term
trDischargeDependentVariable (DependentVariable maybeSelfName maybeVariableName expression') term =
  TrFunctionType input output where
    input = exTranslate expression'
    output self variable =
      let
        term' = case maybeSelfName of
          Nothing -> term
          Just selfName -> trSubstitute selfName self term
      in
        case maybeVariableName of
          Nothing -> term'
          Just variableName -> trSubstitute variableName variable term'

trDischargeVariable :: Variable -> Term -> Term
trDischargeVariable (Variable variableName) term =
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
    ExFunctionType variables body ->
      build trDischargeDependentVariable variables body
    ExFunction variables body ->
      build trDischargeVariable variables body
    ExApplication function arguments ->
      foldl TrApplication (exTranslate function) (fmap exTranslate arguments)
  where
    build discharge variables body =
      foldr discharge (exTranslate body) variables
      
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
      