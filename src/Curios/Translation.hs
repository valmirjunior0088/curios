module Curios.Translation
  (liToTerm
  ,qnToTerm
  ,teDischargePiBinding
  ,teDischargeLambdaBinding
  ,exToTerm
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Expression (..)
  )

import Curios.Term
  (Primitive (..)
  ,Universe (..)
  ,Scope (..)
  ,Term (..)
  ,teAbstract
  )

import Curios.Identifier
  (idNew
  )

import Data.Foldable
  (foldrM
  )

import Control.Monad
  (liftM2
  )

liToTerm :: Literal -> Term
liToTerm =
  TeLiteral

qnToTerm :: QualifiedName -> Term
qnToTerm qualifiedName =
  case qualifiedName of
    QualifiedName [] (Name "type") -> TeType (Universe 0)
    QualifiedName [] (Name "character") -> TePrimitive PrCharacter
    QualifiedName [] (Name "string") -> TePrimitive PrString
    QualifiedName [] (Name "integer") -> TePrimitive PrInteger
    QualifiedName [] (Name "rational") -> TePrimitive PrRational
    _ -> TeFreeVariable qualifiedName

teDischargePiBinding :: PiBinding -> Term -> IO Term
teDischargePiBinding (PiBinding maybeVariableName variableType) term =
  do
    variableType' <- exToTerm variableType
    return (TePiAbstraction variableType' abstractionBody') where
      abstractionBody' =
        case maybeVariableName of
          Nothing -> Scope term
          Just variableName -> teAbstract variableName term

teDischargeLambdaBinding :: LambdaBinding -> Term -> IO Term
teDischargeLambdaBinding (LambdaBinding variableName maybeVariableType) term =
  liftM2 TeLambdaAbstraction variableType' abstractionBody' where
    variableType' =
      case maybeVariableType of
        Nothing ->
          do
            identifier <- idNew
            return (TeMetaVariable identifier (TeType (Universe 0)))
        Just variableType ->
          exToTerm variableType
    abstractionBody' =
      return (teAbstract variableName term)

exToTerm :: Expression -> IO Term
exToTerm expression =
  case expression of
    ExLiteral literal ->
      return (liToTerm literal)
    ExVariable qualifiedName ->
      return (qnToTerm qualifiedName)
    ExPiAbstraction piBindings abstractionBody ->
      do
        abstractionBody' <- exToTerm abstractionBody
        foldrM teDischargePiBinding abstractionBody' piBindings
    ExLambdaAbstraction lambdaBindings abstractionBody ->
      do
        abstractionBody' <- exToTerm abstractionBody
        foldrM teDischargeLambdaBinding abstractionBody' lambdaBindings
    ExApplication function arguments ->
      do
        function' <- exToTerm function
        arguments' <- mapM exToTerm arguments
        return (foldl TeApplication function' arguments')
