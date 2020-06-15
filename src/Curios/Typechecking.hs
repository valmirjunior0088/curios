module Curios.Typechecking
  (prCheck
  ,ltCheck
  ,trCheck
  )
  where

import Curios.Expression
  (Literal (..)
  )

import Curios.Term
  (Primitive (..)
  ,Scope (..)
  ,Term (..)
  ,trInstantiate
  ,trWeaken
  )

import Curios.Universe
  (Universe (..)
  ,unNext
  ,unMax
  )

import Curios.Context
  (Context (..)
  ,coLookup
  )

import Curios.Environment
  (Environment (..)
  ,enInsert
  ,enLookup
  )

import Control.Monad
  (unless
  )

prCheck :: Context -> Environment -> Primitive -> Either String Term
prCheck _ _ _ =
  Right (TrType (Universe 0))

ltCheck :: Context -> Environment -> Literal -> Either String Term
ltCheck _ _ literal =
  Right
    (TrPrimitive
      (case literal of
        LtCharacter _ -> PrCharacter
        LtText _ -> PrText
        LtInteger _ -> PrInteger
        LtRational _ -> PrRational
      )
    )

trNormalize :: Term -> Term
trNormalize term =
  case term of
    TrApplication function argument ->
      case trNormalize function of
        TrAbstraction _ output -> trNormalize (trInstantiate argument output)
        normalizedFunction -> TrApplication normalizedFunction argument

    _ ->
      term

trCheck :: Context -> Environment -> Term -> Either String Term
trCheck context environment term =
  case term of
    TrPrimitive primitive ->
      prCheck context environment primitive

    TrLiteral literal ->
      ltCheck context environment literal

    TrFreeVariable name ->
      coLookup name context

    TrBoundVariable index ->
      enLookup index environment

    TrType universe ->
      Right (TrType (unNext universe))

    TrAbstractionType inputType (Scope output) ->
      do
        inputKind <- trCheck context environment inputType
        inputUniverse <- case trNormalize inputKind of
          TrType universe -> Right universe
          _ -> Left "Invalid input type"
        
        outputType <- trCheck context (enInsert inputType environment) output
        outputUniverse <- case trNormalize outputType of
          TrType universe -> Right universe
          _ -> Left "Invalid output type"

        Right (TrType (unMax inputUniverse outputUniverse))

    TrAbstraction inputType (Scope output) ->
      do
        _ <- trCheck context environment inputType
        outputType <- trCheck context (enInsert inputType environment) output

        let inferredType = TrAbstractionType inputType (Scope (trWeaken outputType))
        _ <- trCheck context environment inferredType

        Right inferredType

    TrApplication function argument ->
      do
        functionType <- trCheck context environment function

        case trNormalize functionType of
          TrAbstractionType inputType output ->
            do
              argumentType <- trCheck context environment argument
              unless (inputType == argumentType) (Left "Ill typed application")

              Right (trInstantiate argument output)
          
          _ ->
            Left "Invalid application"