module Curios.Translation
  (ltToTerm
  ,nmToTerm
  ,trDischarge
  ,exToTerm
  ,cnInsertStatement
  ,cnInsertStatements
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )

import Curios.Term
  (Primitive (..)
  ,Scope (..)
  ,Term (..)
  ,trAbstract
  )

import Curios.Environment
  (enEmpty
  )

import Curios.Context
  (Context (..)
  ,cnInsert
  )

import Curios.Typechecking
  (trInfer
  )

import Curios.Universe
  (Universe (..)
  )

import Data.Foldable
  (foldlM
  )

ltToTerm :: Literal -> Term
ltToTerm =
  TrLiteral

nmToTerm :: Name -> Term
nmToTerm name =
  case name of
    Name "type" -> TrType (Universe 0)
    Name "character" -> TrPrimitive PrCharacter
    Name "text" -> TrPrimitive PrText
    Name "integer" -> TrPrimitive PrInteger
    Name "rational" -> TrPrimitive PrRational
    _ -> TrFreeVariable name

trDischarge :: (Term -> Scope -> Term) -> Binding -> Term -> Term
trDischarge constructAbstraction (Binding variableName variableType) term =
  constructAbstraction (exToTerm variableType) (trAbstract variableName term)

exToTerm :: Expression -> Term
exToTerm expression =
  case expression of
    ExLiteral literal -> ltToTerm literal
    ExVariable name -> nmToTerm name
    ExAbstractionType bindings body -> foldr (trDischarge TrAbstractionType) (exToTerm body) bindings
    ExAbstraction bindings body -> foldr (trDischarge TrAbstraction) (exToTerm body) bindings
    ExApplication function arguments -> foldl TrApplication (exToTerm function) (map exToTerm arguments)

cnInsertStatement :: Context -> Statement -> Either String Context
cnInsertStatement context (StDef name expression) =
  case trInfer context enEmpty (exToTerm expression) of
    Left message ->
      Left ("Failed to typecheck [" ++ show name ++ "]: " ++ message)

    Right term ->
      cnInsert name term context

cnInsertStatements :: [Statement] -> Context -> Either String Context
cnInsertStatements statements context =
  foldlM cnInsertStatement context statements