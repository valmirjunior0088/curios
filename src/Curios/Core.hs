module Curios.Core
  ( Origin (..)
  , Primitive (..)
  , Literal (..)
  , Operator (..)
  , Name
  , Index
  , Type
  , Variable (..)
  , vrUnwrap
  , Term (..)
  , trPrText
  , trPrInteger
  , trPrReal
  , trLtText
  , trLtInteger
  , trLtReal
  , trOpUnary
  , trOpBinary
  , trReference
  , trType
  , trFunctionType
  , trFunction
  , trApplication
  , trShow
  , trOrigin
  , trApplyVariable
  )
  where

import GHC.Natural (Natural)
import Text.Megaparsec (SourcePos)

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

data Variable =
  VrQuote Index |
  VrTerm Term

vrUnwrap :: Variable -> Term
vrUnwrap variable =
  case variable of
    VrQuote index -> trVariable index
    VrTerm term -> term

data Term =
  TrPrimitive Origin Primitive |
  TrLiteral Origin Literal |
  TrOperator Origin Name Operator |
  TrReference Origin Name |
  TrVariable Origin Index |
  TrType Origin |
  TrFunctionType Origin Type (Variable -> Variable -> Type) |
  TrFunction Origin (Variable -> Term) |
  TrApplication Origin Term Term

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

trFunctionType :: Type -> (Variable -> Variable -> Type) -> Term
trFunctionType inputType output =
  TrFunctionType OrMachine inputType output

trFunction :: (Variable -> Term) -> Term
trFunction output =
  TrFunction OrMachine output

trApplication :: Term -> Term -> Term
trApplication function argument =
  TrApplication OrMachine function argument

trShow :: Natural -> Term -> String
trShow depth term =
  case term of
    TrPrimitive _ primitive ->
      "(TrPrimitive " ++ show primitive ++ ")"

    TrLiteral _ literal ->
      "(TrLiteral " ++ show literal ++ ")"

    TrOperator _ name _ ->
      "(TrOperator " ++ show name ++ ")"

    TrReference _ name ->
      "(TrReference " ++ show name ++ ")"

    TrVariable _ index ->
      "(TrVariable " ++ show index ++ ")"

    TrType _ ->
      "(TrType)"

    TrFunctionType _ inputType output ->
      "(TrFunctionType " ++ inputTypeString ++ " " ++ outputString ++ ")" where
        inputTypeString = trShow depth inputType
        self = VrQuote depth
        input = VrQuote (succ depth)
        depth' = succ (succ depth)
        outputString = trShow depth' (output self input)

    TrFunction _ output ->
      "(TrFunction " ++ outputString ++ ")" where
        input = VrQuote depth
        depth' = succ depth
        outputString = trShow depth' (output input)
        
    TrApplication _ function argument ->
      "(TrApplication " ++ functionString ++ " " ++ argumentString ++ ")" where
        functionString = trShow depth function
        argumentString = trShow depth argument

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

trApplyVariable :: Name -> Variable -> Term -> Term
trApplyVariable name variable term =
  case term of
    TrReference origin name' | name == name' ->
      case variable of
        VrQuote index -> TrVariable origin index
        VrTerm image -> image

    TrFunctionType origin inputType output ->
      TrFunctionType origin inputType' output' where
        inputType' = trApplyVariable name variable inputType
        output' self input = trApplyVariable name variable (output self input)

    TrFunction origin output ->
      TrFunction origin output' where
        output' input = trApplyVariable name variable (output input)

    TrApplication origin function argument ->
      TrApplication origin
        (trApplyVariable name variable function)
        (trApplyVariable name variable argument)
        
    term' ->
      term'
