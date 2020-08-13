module Curios.Typechecking
  (ltInfer
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
  ,trWhnf
  )

import Curios.Universe
  (Universe (..)
  ,unNext
  ,unMax
  )

import Curios.Context
  (Context (..)
  ,cnLookup
  )

import Curios.Environment
  (Environment (..)
  ,enInsert
  ,enLookup
  )

import Control.Monad
  (unless
  )

ltInfer :: Literal -> Primitive
ltInfer literal =
  case literal of
    LtCharacter _ -> PrCharacter
    LtText _ -> PrText
    LtInteger _ -> PrInteger
    LtRational _ -> PrRational

trInfer :: Context -> Environment -> Term -> Either String Term
trInfer context environment term =
  case term of
    TrPrimitive primitive ->
      Right (TrType (Universe 0))

    TrLiteral literal ->
      Right (TrPrimitive (ltInfer literal))

    TrFreeVariable name ->
      cnLookup name context

    TrBoundVariable index ->
      enLookup index environment

    TrType universe ->
      Right (TrType (unNext universe))

    TrAbstractionType inputType (Scope outputType) ->
      do
        inputKind <- trInfer context environment inputType
        inputUniverse <- case trWhnf inputKind of
          TrType universe -> Right universe
          term' -> Left ("Invalid input type [" ++ show term' ++ "]")
        
        outputKind <- trInfer context (enInsert inputType environment) outputType
        outputUniverse <- case trWhnf outputKind of
          TrType universe -> Right universe
          term' -> Left ("Invalid output type [" ++ show term' ++ "]")

        Right (TrType (unMax inputUniverse outputUniverse))

    TrAbstraction inputType (Scope outputTerm) ->
      do
        _ <- trInfer context environment inputType
        outputType <- trInfer context (enInsert inputType environment) outputTerm

        let inferredType = TrAbstractionType inputType (Scope outputType)
        _ <- trInfer context environment inferredType

        Right inferredType

    TrApplication function argument ->
      do
        functionType <- trInfer context environment function

        case trWhnf functionType of
          TrAbstractionType inputType outputType ->
            do
              argumentType <- trInfer context environment argument

              unless
                (inputType == argumentType)
                (Left "Ill typed application")

              Right (trInstantiate argument outputType)
          
          _ ->
            Left "Invalid application"