module Curios.Typechecking
  (prInfer
  ,ltInfer
  ,trInfer
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

prInfer :: Context -> Environment -> Primitive -> Either String Term
prInfer _ _ _ =
  Right (TrType (Universe 0))

ltInfer :: Context -> Environment -> Literal -> Either String Term
ltInfer _ _ literal =
  Right
    (TrPrimitive
      (case literal of
        LtCharacter _ -> PrCharacter
        LtText _ -> PrText
        LtInteger _ -> PrInteger
        LtRational _ -> PrRational
      )
    )

trWeakNormalize :: Term -> Term
trWeakNormalize term =
  case term of
    TrApplication function argument ->
      case trWeakNormalize function of
        TrAbstraction _ output -> trWeakNormalize (trInstantiate argument output)
        normalizedFunction -> TrApplication normalizedFunction argument

    _ ->
      term

trInfer :: Context -> Environment -> Term -> Either String Term
trInfer context environment term =
  case term of
    TrPrimitive primitive ->
      prInfer context environment primitive

    TrLiteral literal ->
      ltInfer context environment literal

    TrFreeVariable name ->
      coLookup name context

    TrBoundVariable index ->
      enLookup index environment

    TrType universe ->
      Right (TrType (unNext universe))

    TrAbstractionType inputType (Scope output) ->
      do
        inputKind <- trInfer context environment inputType
        inputUniverse <- case trWeakNormalize inputKind of
          TrType universe -> Right universe
          _ -> Left "Invalid input type"
        
        outputType <- trInfer context (enInsert inputType environment) output
        outputUniverse <- case trWeakNormalize outputType of
          TrType universe -> Right universe
          _ -> Left "Invalid output type"

        Right (TrType (unMax inputUniverse outputUniverse))

    TrAbstraction inputType (Scope output) ->
      do
        _ <- trInfer context environment inputType
        outputType <- trInfer context (enInsert (trWeaken inputType) environment) output

        let inferredType = TrAbstractionType inputType (Scope outputType)
        _ <- trInfer context environment inferredType

        Right inferredType

    TrApplication function argument ->
      do
        functionType <- trInfer context environment function

        case trWeakNormalize functionType of
          TrAbstractionType inputType output ->
            do
              argumentType <- trInfer context environment argument
              unless (inputType == argumentType) (Left "Ill typed application")

              Right (trInstantiate argument output)
          
          _ ->
            Left "Invalid application"