module Curios.Core.Term
  (Origin (..)
  ,Primitive (..)
  ,Literal (..)
  ,Operator (..)
  ,Name
  ,Index
  ,Type
  ,Argument (..)
  ,Depth
  ,Term (..)
  ,arUnwrap
  ,trPrimitive
  ,trPrText
  ,trPrInteger
  ,trPrReal
  ,trLiteral
  ,trLtText
  ,trLtInteger
  ,trLtReal
  ,trOperator
  ,trOpUnary
  ,trOpBinary
  ,trReference
  ,trVariable
  ,trType
  ,trFunctionType
  ,trFunction
  ,trApplication
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
  LtInteger Int |
  LtReal Double
  deriving (Eq, Show)

data Operator =
  OpUnary (Literal -> Term) |
  OpBinary (Literal -> Literal -> Term)

type Name =
  String

type Index =
  Natural

type Type =
  Term

data Argument =
  ArQuote Index |
  ArTerm Term

type Depth =
  Natural

data Term =
  TrPrimitive Origin Primitive |
  TrLiteral Origin Literal |
  TrOperator Origin Name Operator |
  TrReference Origin Name |
  TrVariable Origin Index |
  TrType Origin |
  TrFunctionType Origin Type (Argument -> Argument -> Term) |
  TrFunction Origin (Argument -> Term) |
  TrApplication Origin Term Term

arUnwrap :: Argument -> Term
arUnwrap argument =
  case argument of
    ArQuote index -> trVariable index
    ArTerm term -> term

trPrimitive :: Primitive -> Term
trPrimitive primitive =
  TrPrimitive OrMachine primitive

trPrText :: Term
trPrText = 
  trPrimitive PrText

trPrInteger :: Term
trPrInteger =
  trPrimitive PrInteger

trPrReal :: Term
trPrReal =
  trPrimitive PrReal

trLiteral :: Literal -> Term
trLiteral literal =
  TrLiteral OrMachine literal

trLtText :: String -> Term
trLtText text =
  trLiteral (LtText text)

trLtInteger :: Int -> Term
trLtInteger int =
  trLiteral (LtInteger int)

trLtReal :: Double -> Term
trLtReal real =
  trLiteral (LtReal real)

trOperator :: Name -> Operator -> Term
trOperator name operator =
  TrOperator OrMachine name operator

trOpUnary :: Name -> (Literal -> Term) -> Term
trOpUnary name operator =
  trOperator name (OpUnary operator)

trOpBinary :: Name -> (Literal -> Literal -> Term) -> Term
trOpBinary name operator =
  trOperator name (OpBinary operator)

trReference :: Name -> Term
trReference name =
  TrReference OrMachine name

trVariable :: Index -> Term
trVariable index =
  TrVariable OrMachine index

trType :: Term
trType =
  TrType OrMachine

trFunctionType :: Type -> (Argument -> Argument -> Term) -> Term
trFunctionType input output =
  TrFunctionType OrMachine input output

trFunction :: (Argument -> Term) -> Term
trFunction output =
  TrFunction OrMachine output

trApplication :: Term -> Term -> Term
trApplication function argument =
  TrApplication OrMachine function argument

trOrigin :: Term -> Origin
trOrigin term =
  case term of
    TrPrimitive origin _ -> origin
    TrLiteral origin _ -> origin
    TrOperator origin _ _ -> origin
    TrReference origin _ -> origin
    TrVariable origin _ -> origin
    TrType origin -> origin
    TrFunctionType origin _ _ -> origin
    TrFunction origin _ -> origin
    TrApplication origin _ _ -> origin

trAbstract :: Name -> Index -> Term -> Term
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
    term' ->
      term'

trSubstitute :: Name -> Term -> Term -> Term
trSubstitute name image term =
  case term of
    TrReference _ name' | name == name' ->
      image
    TrFunctionType origin input output ->
      TrFunctionType origin input' output' where
        input' = trSubstitute name image input
        output' self variable = trSubstitute name image (output self variable)
    TrFunction origin output ->
      TrFunction origin output' where
        output' variable = trSubstitute name image (output variable)
    TrApplication origin function argument ->
      TrApplication origin
        (trSubstitute name image function)
        (trSubstitute name image argument)
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
        TrOperator _ name _ ->
          "(TrOperator \"" ++ name ++ "\")"
        TrReference _ name ->
          "(TrReference \"" ++ name ++ "\")"
        TrVariable _ index ->
          "(TrVariable " ++ show index ++ ")"
        TrType _ ->
          "(TrType)"
        TrFunctionType _ input output ->
          "(TrFunctionType "
            ++ "(Self " ++ show (depth + 0) ++ ") "
            ++ "(Variable " ++ show (depth + 1) ++ ": " ++ go (depth + 0) input ++ ") "
            ++ "{ " ++ go (depth + 2) (output (ArQuote (depth + 0)) (ArQuote (depth + 1))) ++ " })"
        TrFunction _ output ->
          "(TrFunction "
            ++ "(Variable " ++ show (depth + 0) ++ ") "
            ++ "{ " ++ go (depth + 1) (output (ArQuote (depth + 0))) ++ " })"
        TrApplication _ function argument ->
          "(TrApplication " ++ go (depth + 0) function ++ " " ++ go (depth + 0) argument ++ ")"
