module Curios.Translation
  (ltToTerm
  ,nmToTerm
  ,trDischarge
  ,exToTerm
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,Binding (..)
  ,Expression (..)
  )

import Curios.Term
  (Primitive (..)
  ,Scope (..)
  ,Term (..)
  ,trAbstract
  )

import Curios.Universe
  (Universe (..)
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
