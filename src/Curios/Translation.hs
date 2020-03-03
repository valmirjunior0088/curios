module Curios.Translation
  ( qnTranslate
  , atTranslate
  , teAbstract
  , teApplyPiBinding
  , teApplyLambdaBinding
  , abTranslate
  , exTranslate
  )
  where

import Curios.Expression
  ( QualifiedName
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  )

import Curios.Term
  ( Primitive (..)
  , Literal (..)
  , Term (..)
  , teAbstract
  )

import Control.Monad
  ( liftM2
  )

import Data.Unique
  ( newUnique
  )

import Data.Foldable
  ( foldrM
  )

qnTranslate :: QualifiedName -> Term
qnTranslate qualifiedName =
  case qualifiedName of
    ["type"] -> TeType 0
    ["character"] -> TePrimitive PrCharacter
    ["string"] -> TePrimitive PrString
    ["integer"] -> TePrimitive PrInteger
    ["rational"] -> TePrimitive PrRational
    _ -> TeFreeVariable qualifiedName

atTranslate :: Atom -> Term
atTranslate atom =
  case atom of
    AtSymbol qualifiedName -> qnTranslate qualifiedName
    AtCharacter char -> TeLiteral (LiCharacter char)
    AtString string -> TeLiteral (LiString string)
    AtInteger integer -> TeLiteral (LiInteger integer)
    AtRational double -> TeLiteral (LiRational double)

teApplyPiBinding :: PiBinding -> Term -> IO Term
teApplyPiBinding (PiBinding maybeVariableName variableType) term =
  do
    variableType' <- exTranslate variableType
    return (TePiAbstraction variableType' abstractionBody') where
      abstractionBody' =
        case maybeVariableName of
          Nothing -> term
          Just variableName -> teAbstract variableName term

teApplyLambdaBinding :: LambdaBinding -> Term -> IO Term
teApplyLambdaBinding (LambdaBinding variableName maybeVariableType) term =
  liftM2 TeLambdaAbstraction variableType' abstractionBody' where
    variableType' =
      case maybeVariableType of
        Nothing ->
          do
            unique <- newUnique
            return (TeMetaVariable unique (TeType 0))
        Just variableType ->
          exTranslate variableType
    abstractionBody' =
      return (teAbstract variableName term)

abTranslate :: (binding -> Term -> IO Term) -> Abstraction binding -> IO Term
abTranslate applyBinding (Abstraction bindings abstractionBody) =
  do
    abstractionBody' <- exTranslate abstractionBody
    foldrM applyBinding abstractionBody' bindings

exTranslate :: Expression -> IO Term
exTranslate expression =
  case expression of
    ExAtom atom ->
      return (atTranslate atom)
    ExPiAbstraction piAbstraction ->
      abTranslate teApplyPiBinding piAbstraction 
    ExLambdaAbstraction lambdaAbstraction ->
      abTranslate teApplyLambdaBinding lambdaAbstraction
    ExApplication function arguments ->
      do
        function' <- exTranslate function
        arguments' <- mapM exTranslate arguments
        return (foldl TeApplication function' arguments')

