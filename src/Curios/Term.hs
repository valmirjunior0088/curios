module Curios.Term
  ( Universe (..)
  , Index (..)
  , Type (..)
  , Primitive (..)
  , Literal (..)
  , Term (..)
  , idTranslate
  , atTranslate
  , teAbstract
  , teApplyPiBinding
  , teApplyLambdaBinding
  , abTranslate
  , exTranslate
  )
  where

import Curios.Expression
  ( Name (..)
  , Identifier (..)
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  )

import Control.Monad
  ( liftM2
  )

import Data.Unique
  ( Unique (..)
  , newUnique
  )

import Data.Foldable
  ( foldrM
  )

type Universe =
  Integer

type Index =
  Integer

type Type =
  Term
  
data Primitive =
  PrCharacter |
  PrString |
  PrInteger |
  PrRational

data Literal =
  LiCharacter Char |
  LiString String |
  LiInteger Integer |
  LiRational Double

data Term =
  TeType Universe |
  TePiAbstraction Type Term |
  TeLambdaAbstraction Type Term |
  TeApplication Term Term |
  TeFreeVariable Identifier |
  TeBoundVariable Index |
  TeMetaVariable Unique Type |
  TePrimitive Primitive |
  TeLiteral Literal

idTranslate :: Identifier -> Term
idTranslate identifier =
  case identifier of
    ["type"] -> TeType 0
    ["character"] -> TePrimitive PrCharacter
    ["string"] -> TePrimitive PrString
    ["integer"] -> TePrimitive PrInteger
    ["rational"] -> TePrimitive PrRational
    _ -> TeFreeVariable identifier

atTranslate :: Atom -> Term
atTranslate atom =
  case atom of
    AtSymbol identifier -> idTranslate identifier
    AtCharacter char -> TeLiteral (LiCharacter char)
    AtString string -> TeLiteral (LiString string)
    AtInteger integer -> TeLiteral (LiInteger integer)
    AtRational double -> TeLiteral (LiRational double)

teAbstract :: Name -> Term -> Term
teAbstract name term =
  go 0 term where
    go depth term =
      case term of
        TePiAbstraction variableType abstractionBody ->
          TePiAbstraction (go depth variableType) (go (depth + 1) abstractionBody)
        TeLambdaAbstraction variableType abstractionBody ->
          TeLambdaAbstraction (go depth variableType) (go (depth + 1) abstractionBody)
        TeApplication function argument ->
          TeApplication (go depth function) (go depth argument)
        TeMetaVariable unique variableType ->
          TeMetaVariable unique (go depth variableType)
        TeFreeVariable [name'] | name == name' ->
          TeBoundVariable depth
        _ ->
          term

teApplyPiBinding :: PiBinding -> Term -> IO Term
teApplyPiBinding (PiBinding maybeVariableName variableType) term =
  do
    variableType <- exTranslate variableType
    return (TePiAbstraction variableType abstractionBody) where
      abstractionBody =
        case maybeVariableName of
          Nothing -> term
          Just variableName -> teAbstract variableName term

teApplyLambdaBinding :: LambdaBinding -> Term -> IO Term
teApplyLambdaBinding (LambdaBinding variableName maybeVariableType) term =
  liftM2 TeLambdaAbstraction variableType abstractionBody where
    variableType =
      case maybeVariableType of
        Nothing ->
          do
            unique <- newUnique
            return (TeMetaVariable unique (TeType 0))
        Just variableType ->
          exTranslate variableType
    abstractionBody =
      return (teAbstract variableName term)

abTranslate :: (binding -> Term -> IO Term) -> Abstraction binding -> IO Term
abTranslate applyBinding (Abstraction bindings abstractionBody) =
  do
    abstractionBody <- exTranslate abstractionBody
    foldrM applyBinding abstractionBody bindings

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
        function <- exTranslate function
        arguments <- mapM exTranslate arguments
        return (foldl TeApplication function arguments)
