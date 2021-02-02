module Curios.Core.Term
  (Origin (..)
  ,Primitive (..)
  ,Literal (..)
  ,Name
  ,Type
  ,Argument (..)
  ,Term (..)
  ,trOrigin
  ,trAbstract
  ,trSubstitute
  ,showTerm
  )
  where

import GHC.Natural (Natural (..))
import Text.Megaparsec (SourcePos (..))

data Origin =
  OrMachine |
  OrSource SourcePos

data Primitive =
  PrText |
  PrInteger |
  PrReal
  deriving (Eq, Show)

data Literal =
  LtText String |
  LtInteger Integer |
  LtReal Double
  deriving (Eq, Show)

type Name =
  String

type Type =
  Term

data Argument =
  ArPlaceholder Natural |
  ArTerm Term

data Term =
  TrPrimitive Origin Primitive |
  TrLiteral Origin Literal |
  TrReference Origin Name |
  TrVariable Origin Natural |
  TrType Origin |
  TrFunctionType Origin Type (Argument -> Argument -> Term) |
  TrFunction Origin (Argument -> Term) |
  TrApplication Origin Term Term |
  TrAnnotated Origin Type Term

instance Eq Term where
  (==) =
    go 0 where
      go depth one other =
        case (one, other) of
          (TrPrimitive _ primitive, TrPrimitive _ primitive') ->
            primitive == primitive'
          (TrLiteral _ literal, TrLiteral _ literal') ->
            literal == literal'
          (TrReference _ name, TrReference _ name') ->
            name == name'
          (TrVariable _ index, TrVariable _ index') ->
            index == index'
          (TrType _, TrType _) ->
            True
          (TrFunctionType _ input output, TrFunctionType _ input' output') ->
            (&&)
              (go (depth + 0) input input')
              (go (depth + 2)
                (output (ArPlaceholder (depth + 0)) (ArPlaceholder (depth + 1)))
                (output' (ArPlaceholder (depth + 0)) (ArPlaceholder (depth + 1)))
              )
          (TrFunction _ output, TrFunction _ output') ->
            go (depth + 1)
              (output (ArPlaceholder (depth + 0)))
              (output' (ArPlaceholder (depth + 0)))
          (TrApplication _ function argument, TrApplication _ function' argument') ->
            (&&)
              (go (depth + 0) function function')
              (go (depth + 0) argument argument')
          (TrAnnotated _ termType term, TrAnnotated _ termType' term') ->
            (&&)
              (go (depth + 0) termType termType')
              (go (depth + 0) term term')
          _ ->
            False

trOrigin :: Term -> Origin
trOrigin term =
  case term of
    TrPrimitive origin _ -> origin
    TrLiteral origin _ -> origin
    TrReference origin _ -> origin
    TrVariable origin _ -> origin
    TrType origin -> origin
    TrFunctionType origin _ _ -> origin
    TrFunction origin _ -> origin
    TrApplication origin _ _ -> origin
    TrAnnotated origin _ _ -> origin

trAbstract :: Name -> Natural -> Term -> Term
trAbstract name index term =
  case term of
    TrReference origin name' | name == name' ->
      TrVariable origin index
    TrFunctionType origin input output ->
      TrFunctionType origin input' output' where
        input' = trAbstract name index input
        output' self variable = trAbstract name index (output self variable)
    TrFunction origin output ->
      TrFunction origin output' where
        output' variable = trAbstract name index (output variable)
    TrApplication origin function argument ->
      TrApplication origin
        (trAbstract name index function)
        (trAbstract name index argument)
    TrAnnotated origin termType term' ->
      TrAnnotated origin
        (trAbstract name index termType)
        (trAbstract name index term')
    term' ->
      term'

trSubstitute :: Name -> Term -> Term -> Term
trSubstitute name source term =
  case term of
    TrReference _ name' | name == name' ->
      source
    TrFunctionType origin input output ->
      TrFunctionType origin input' output' where
        input' = trSubstitute name source input
        output' self variable = trSubstitute name source (output self variable)
    TrFunction origin output ->
      TrFunction origin output' where
        output' variable = trSubstitute name source (output variable)
    TrApplication origin function argument ->
      TrApplication origin
        (trSubstitute name source function)
        (trSubstitute name source argument)
    TrAnnotated origin termType term' ->
      TrAnnotated origin
        (trSubstitute name source termType)
        (trSubstitute name source term')
    term' ->
      term'

showTerm :: Term -> String
showTerm =
  go 0 where
    go depth term =
      case term of
        TrPrimitive _ primitive ->
          "(TrPrimitive (" ++ show primitive ++ "))"
        TrLiteral _ literal ->
          "(TrLiteral (" ++ show literal ++ "))"
        TrReference _ name ->
          "(TrReference " ++ show name ++ ")"
        TrVariable _ index ->
          "(TrVariable " ++ show index ++ ")"
        TrType _ ->
          "(TrType)"
        TrFunctionType _ input output ->
          "(TrFunctionType "
            ++ "(Self " ++ show (depth + 0) ++ ") "
            ++ "(Variable " ++ show (depth + 1) ++ ": " ++ go (depth + 0) input ++ ") "
            ++ "{ " ++ go (depth + 2) (output (ArPlaceholder (depth + 0)) (ArPlaceholder (depth + 1))) ++ " })"
        TrFunction _ output ->
          "(TrFunction "
            ++ "(Variable " ++ show (depth + 0) ++ ") "
            ++ "{ " ++ go (depth + 1) (output (ArPlaceholder (depth + 0))) ++ " })"
        TrApplication _ function argument ->
          "(TrApplication " ++ go (depth + 0) function ++ " " ++ go (depth + 0) argument ++ ")"
        TrAnnotated _ termType term' ->
          "(TrAnnotated " ++ go (depth + 0) termType ++ " " ++ go (depth + 0) term' ++ ")"
