module Curios.Term
  (Primitive (..)
  ,Constant (..)
  ,Type
  ,Name (..)
  ,Scope (..)
  ,Term (..)
  ,trShift
  ,trSubstitute
  ,trWhnf
  ,trBetaReduce
  ,Definition (..)
  ,dfIdentifier
  ,dfLookup
  )
  where

import Curios.Expression
  (Literal (..)
  ,Identifier
  )

import Data.List
  (find
  )

data Primitive =
  PrCharacter |
  PrText |
  PrInteger |
  PrRational
  deriving (Eq, Show)

data Constant =
  CtKind |
  CtType
  deriving (Eq, Show)

type Type =
  Term

data Name =
  Name Identifier Integer
  deriving (Eq, Ord)

instance Show Name where
  show (Name identifier index) =
    if index == 0
      then identifier
      else identifier ++ "@" ++ show index

newtype Scope =
  Scope Term
  deriving (Eq, Show)

data Term =
  TrPrimitive Primitive |
  TrLiteral Literal |
  TrConstant Constant |
  TrAbstractionType Identifier Type Scope |
  TrAbstraction Identifier Type Scope |
  TrApplication Term Term |
  TrVariable Name
  deriving (Eq, Show)

trShift :: Integer -> Identifier -> Term -> Term
trShift amount identifier =
  go 0 where
    go depth term =
      let depth' identifier' = if identifier == identifier' then succ depth else depth in
        case term of
          TrPrimitive primitive -> TrPrimitive primitive
          TrLiteral literal -> TrLiteral literal
          TrConstant constant -> TrConstant constant

          TrAbstractionType identifier' input (Scope output) ->
            TrAbstractionType identifier' (go depth input) (Scope (go (depth' identifier') output))
          
          TrAbstraction identifier' input (Scope output) ->
            TrAbstraction identifier' (go depth input) (Scope (go (depth' identifier') output))
          
          TrApplication function argument ->
            TrApplication (go depth function) (go depth argument)
          
          TrVariable (Name identifier' index) ->
            let
              index' =
                if identifier == identifier' && index >= depth
                  then index + amount
                  else index
            in
              TrVariable (Name identifier' index')

trSubstitute :: Name -> Term -> Term -> Term
trSubstitute name@(Name identifier index) image target =
  let
    trSubstituteUnderBinder construct identifier' input (Scope output) =
      let
        input' = trSubstitute name image input
        name' = if identifier == identifier' then Name identifier (succ index) else name
        output' = trSubstitute name' (trShift 1 identifier' image) output
      in
        construct identifier' input' (Scope output')
  in
    case target of
      TrPrimitive primitive -> TrPrimitive primitive
      TrLiteral literal -> TrLiteral literal
      TrConstant constant -> TrConstant constant

      TrAbstractionType identifier' input scope ->
        trSubstituteUnderBinder TrAbstractionType identifier' input scope
      
      TrAbstraction identifier' input scope ->
        trSubstituteUnderBinder TrAbstraction identifier' input scope
      
      TrApplication function argument ->
        TrApplication (trSubstitute name image function) (trSubstitute name image argument)
      
      TrVariable name' ->
        if name == name' then image else TrVariable name'

trWhnf :: Term -> Term
trWhnf term =
  case term of
    TrApplication function argument ->
      case trWhnf function of
        TrAbstraction identifier _ scope -> trBetaReduce identifier argument scope
        function' -> TrApplication function' argument

    _ ->
      term

trBetaReduce :: Identifier -> Term -> Scope -> Term
trBetaReduce identifier image (Scope output) =
  let
    image' = trShift 1 identifier image
    output' = trSubstitute (Name identifier 0) image' output
  in
    trWhnf (trShift (-1) identifier output')

data Definition =
  Definition Identifier Type Term
  deriving (Show)

dfIdentifier :: Definition -> Identifier
dfIdentifier (Definition identifier _ _) =
  identifier

dfLookup :: Identifier -> [Definition] -> Maybe Definition
dfLookup identifier definitions =
  find ((identifier ==) . dfIdentifier) definitions
