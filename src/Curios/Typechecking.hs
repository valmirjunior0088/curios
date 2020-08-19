module Curios.Typechecking
  (ltSynthesiseType
  ,ctAxiom
  ,ctRule
  ,trIsWellTypedWith
  ,trSynthesiseTypeWith
  ,trSynthesiseTypeOf
  )
  where

import Curios.Expression
  (Literal (..)
  )

import Curios.Term
  (Primitive (..)
  ,Constant (..)
  ,Type
  ,Scope (..)
  ,Term (..)
  ,trWhnf
  ,trBetaReduce
  )

import Curios.Context
  (Context
  ,cnEmpty
  ,cnInsert
  ,cnLookup
  )

import Control.Monad
  (unless
  )

ltSynthesiseType :: Literal -> Primitive
ltSynthesiseType literal =
  case literal of
    LtCharacter _ -> PrCharacter
    LtText _ -> PrText
    LtInteger _ -> PrInteger
    LtRational _ -> PrRational

ctAxiom :: Constant -> Either String Constant
ctAxiom constant =
  case constant of
    CtKind -> Left "Sole superkind is untyped"
    CtType -> Right CtKind

ctRule :: Constant -> Constant -> Constant
ctRule constant constant' =
  case (constant, constant') of
    (CtKind, CtKind) -> CtKind
    (CtKind, CtType) -> CtType
    (CtType, CtKind) -> CtKind
    (CtType, CtType) -> CtType

trIsWellTypedWith :: Context Term -> Term -> Either String ()
trIsWellTypedWith context term =
  trSynthesiseTypeWith context term >> Right ()

trSynthesiseTypeWith :: Context Term -> Term -> Either String Type
trSynthesiseTypeWith context term =
  case term of
    TrPrimitive _ -> Right (TrConstant CtType)
    TrLiteral literal -> Right (TrPrimitive (ltSynthesiseType literal))
    TrConstant constant -> ctAxiom constant >>= Right . TrConstant

    TrAbstractionType identifier input (Scope output) ->
      do
        inputKind <- trSynthesiseTypeWith context input >>= Right . trWhnf

        inputKind' <- case inputKind of
          TrConstant constant -> Right constant
          _ -> Left ("Input of pi-type is not a type [" ++ show input ++ "]")

        outputKind <- trSynthesiseTypeWith (cnInsert identifier input context) output >>= Right . trWhnf

        outputKind' <- case outputKind of
          TrConstant constant -> Right constant
          _ -> Left ("Output of pi-type is not a type [" ++ show output ++ "]")

        Right (TrConstant (ctRule inputKind' outputKind'))

    TrAbstraction identifier input (Scope output) ->
      do
        trIsWellTypedWith context input
        outputType <- trSynthesiseTypeWith (cnInsert identifier input context) output
        let abstractionType = TrAbstractionType identifier input (Scope outputType)
        trIsWellTypedWith context abstractionType
        
        Right abstractionType
    
    TrApplication function argument ->
      do
        functionType <- trSynthesiseTypeWith context function >>= Right . trWhnf

        case functionType of
          TrAbstractionType identifier input scope ->
            do
              argumentType <- trSynthesiseTypeWith context argument

              unless
                (input == argumentType)
                (Left ("Ill-typed application [" ++ show input ++ "] [" ++ show argumentType ++ "]"))

              Right (trBetaReduce identifier argument scope)

          _ ->
            Left ("Left-hand side of application is not a function [" ++ show function ++ "]")
    
    TrVariable name ->
      case cnLookup name context of
        Nothing -> Left ("Unbound variable [" ++ show name ++ "]")
        Just term' -> Right term'

trSynthesiseTypeOf :: Term -> Either String Type
trSynthesiseTypeOf term =
  trSynthesiseTypeWith cnEmpty term
