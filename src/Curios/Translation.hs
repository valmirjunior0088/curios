module Curios.Translation
  (idToTerm
  ,trDischarge
  ,exToTerm
  ,trDefineIn
  ,trDefineManyIn
  ,dfInsert
  ,dfInsertMany
  ,stToDefinitions
  )
  where

import Curios.Expression
  (Identifier
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )

import Curios.Term
  (Primitive (..)
  ,Constant (..)
  ,Type
  ,Name (..)
  ,Scope (..)
  ,Term (..)
  ,Definition (..)
  ,trWhnf
  ,trBetaReduce
  )

import Curios.Typechecking
  (trSynthesiseTypeOf
  )

import Data.Foldable
  (foldlM
  )

idToTerm :: Identifier -> Term
idToTerm identifier =
  case identifier of
    "character" -> TrPrimitive PrCharacter
    "text" -> TrPrimitive PrText
    "integer" -> TrPrimitive PrInteger
    "rational" -> TrPrimitive PrRational
    "kind" -> TrConstant CtKind
    "type" -> TrConstant CtType
    _ -> TrVariable (Name identifier 0)

trDischarge :: (Identifier -> Type -> Scope -> Term) -> Binding -> Term -> Term
trDischarge construct (Binding identifier expression) term =
  construct identifier (exToTerm expression) (Scope term)

exToTerm :: Expression -> Term
exToTerm expression =
  case expression of
    ExLiteral literal -> TrLiteral literal
    ExAbstractionType bindings body -> foldr (trDischarge TrAbstractionType) (exToTerm body) bindings
    ExAbstraction bindings body -> foldr (trDischarge TrAbstraction) (exToTerm body) bindings
    ExApplication function arguments -> foldl TrApplication (exToTerm function) (map exToTerm arguments)
    ExIdentifier identifier -> idToTerm identifier

trDefineIn :: Definition -> Term -> Term
trDefineIn (Definition identifier _ domain) term =
  trBetaReduce identifier domain (Scope term)

trDefineManyIn :: [Definition] -> Term -> Term
trDefineManyIn definitions term =
  foldr trDefineIn term definitions

dfInsert :: [Definition] -> Statement -> Either String [Definition]
dfInsert definitions (StDef identifier expression) =
  do
    let output = exToTerm expression
    input <- trSynthesiseTypeOf (trDefineManyIn definitions output)

    Right (definitions ++ [Definition identifier input (trWhnf output)])

dfInsertMany :: [Definition] -> [Statement] -> Either String [Definition]
dfInsertMany definitions statements =
  foldlM dfInsert definitions statements

stToDefinitions :: [Statement] -> Either String [Definition]
stToDefinitions statements =
  dfInsertMany [] statements
