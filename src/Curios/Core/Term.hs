module Curios.Core.Term
  (PrimitiveType (..)
  ,Type
  ,Term (..)
  )
  where

import GHC.Natural (Natural (..))
import Curios.Syntax.Expression (Name (..), Primitive (..))

data PrimitiveType =
  PtText |
  PtInteger |
  PtRational
  deriving (Show, Eq)

type Type =
  Term

data Term =
  TrVariable Natural |
  TrReference Name |
  TrType |
  TrPrimitiveType PrimitiveType |
  TrPrimitive Primitive |
  TrFunctionType Type (Term -> Term -> Term) |
  TrFunction (Term -> Term) |
  TrApplication Term Term |
  TrAnnotated Type Term

instance Show Term where
  show =
    go 0 where
      go depth term =
        case term of
          TrVariable index ->
            "(TrVariable " ++ show index ++ ")"
          TrReference name ->
            "(TrReference " ++ show name ++ ")"
          TrType ->
            "(TrType)"
          TrPrimitiveType primitiveType ->
            "(TrPrimitiveType (" ++ show primitiveType ++ "))"
          TrPrimitive primitive ->
            "(TrPrimitive (" ++ show primitive ++ "))"
          TrFunctionType input output ->
            "(TrFunctionType "
              ++ "(Self " ++ show (depth + 0) ++ ") "
              ++ "(Variable " ++ show (depth + 1) ++ ": " ++ go (depth + 0) input ++ ") "
              ++ "{ " ++ go (depth + 2) (output (TrVariable (depth + 0)) (TrVariable (depth + 1))) ++ " })"
          TrFunction output ->
            "(TrFunction "
              ++ "(Variable " ++ show (depth + 0) ++ ") "
              ++ "{ " ++ go (depth + 1) (output (TrVariable (depth + 0))) ++ " })"
          TrApplication function argument ->
            "(TrApplication " ++ go (depth + 0) function ++ " " ++ go (depth + 0) argument ++ ")"
          TrAnnotated termType term' ->
            "(TrAnnotated " ++ go (depth + 0) termType ++ " " ++ go (depth + 0) term' ++ ")"
  
instance Eq Term where
  (==) =
    go 0 where
      go depth one other =
        case (one, other) of
          (TrVariable index, TrVariable index') ->
            index == index'
          (TrReference name, TrReference name') ->
            name == name'
          (TrType, TrType) ->
            True
          (TrPrimitiveType primitiveType, TrPrimitiveType primitiveType') ->
            primitiveType == primitiveType'
          (TrPrimitive primitive, TrPrimitive primitive') ->
            primitive == primitive'
          (TrFunctionType input output, TrFunctionType input' output') ->
            (&&)
              (go (depth + 0) input input')
              (go
                (depth + 2)
                (output (TrVariable (depth + 0)) (TrVariable (depth + 1)))
                (output' (TrVariable (depth + 0)) (TrVariable (depth + 1)))
              )
          (TrFunction output, TrFunction output') ->
            go (depth + 1) (output (TrVariable (depth + 0))) (output' (TrVariable (depth + 0)))
          (TrApplication function argument, TrApplication function' argument') ->
            (&&)
              (go (depth + 0) function function')
              (go (depth + 0) argument argument')
          (TrAnnotated termType term, TrAnnotated termType' term') ->
            (&&)
              (go (depth + 0) termType termType')
              (go (depth + 0) term term')
          _ ->
            False
