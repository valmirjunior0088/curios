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
    "kind" -> TrConstant CtKind
    "type" -> TrConstant CtType
    "character" -> TrPrimitive PrCharacter
    "text" -> TrPrimitive PrText
    "integer" -> TrPrimitive PrInteger
    "rational" -> TrPrimitive PrRational
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
trDefineIn (Definition identifier range domain) term =
  trWhnf (TrApplication (TrAbstraction identifier range (Scope term)) domain)

trDefineManyIn :: [Definition] -> Term -> Term
trDefineManyIn definitions term =
  foldr trDefineIn term definitions

dfInsert :: [Definition] -> Statement -> Either String [Definition]
dfInsert definitions (StDef identifier expression) =
  do
    let domain = exToTerm expression
    range <- trSynthesiseTypeOf (trDefineManyIn definitions domain)

    Right (definitions ++ [Definition identifier (trWhnf range) (trWhnf domain)])

dfInsertMany :: [Definition] -> [Statement] -> Either String [Definition]
dfInsertMany definitions statements =
  foldlM dfInsert definitions statements

stToDefinitions :: [Statement] -> Either String [Definition]
stToDefinitions statements =
  dfInsertMany [] statements
