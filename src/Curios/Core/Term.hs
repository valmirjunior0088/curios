module Curios.Core.Term
  ( Origin
  , Name
  , Index
  , Type
  , Primitive (..)
  , Literal (..)
  , Operation (..)
  , Term (..)
  , getOrigin
  , shift
  , instantiate
  , trReference
  , trVariable
  , trType
  , trFunctionType
  , trFunction
  , trApplication
  , trSelf
  , trData
  , trCase
  , trPrimitive
  , trLiteral
  , trOperation
  )
  where

import Data.Int (Int32)
import Data.List (intercalate)
import Text.Megaparsec (SourcePos)

type Origin = Maybe SourcePos

type Name = String
type Index = Int
type Type = Term

data Primitive =
  PrInt32 |
  PrFlt32
  deriving (Show, Eq)

data Literal =
  LtInt32 Int32 |
  LtFlt32 Float
  deriving (Show, Eq)

data Operation =
  OpInt32Sum |
  OpFlt32Sum
  deriving (Show, Eq)

data Term =
  TrReference Origin Name |
  TrVariable Origin Index |
  TrType Origin |
  TrFunctionType Origin Type Type |
  TrFunction Origin Term |
  TrApplication Origin Term Term |
  TrSelf Origin Type |
  TrData Origin Term |
  TrCase Origin Term |
  TrPrimitive Origin Primitive |
  TrLiteral Origin Literal |
  TrOperation Origin Operation [Term]

instance Show Term where
  show term = case term of
    TrReference _ name ->
      "TrReference " ++ name

    TrVariable _ index ->
      "TrVariable " ++ show index

    TrType _ ->
      "TrType"

    TrFunctionType _ input output ->
      "TrFunctionType (" ++ show input ++ ") (" ++ show output ++ ")"

    TrFunction _ output ->
      "TrFunction (" ++ show output ++ ")"

    TrApplication _ function argument ->
      "TrApplication (" ++ show function ++ ") (" ++ show argument ++ ")"

    TrSelf _ output -> 
      "TrSelf (" ++ show output ++ ")"

    TrData _ constructor ->
      "TrData (" ++ show constructor ++ ")"

    TrCase _ scrutinee ->
      "TrCase (" ++ show scrutinee ++ ")"

    TrPrimitive _ primitive ->
      "TrPrimitive (" ++ show primitive ++ ")"

    TrLiteral _ literal ->
      "TrLiteral (" ++ show literal ++ ")"

    TrOperation _ operation arguments ->
      "TrOperation " ++ show operation ++ " [" ++ intercalate ", " (map show arguments)

instance Eq Term where
  one == other =
    case (one, other) of
      (TrReference _ name, TrReference _ name') ->
        name == name'

      (TrVariable _ index, TrVariable _ index') ->
        index == index'

      (TrType _, TrType _) ->
        True

      (TrFunctionType _ input output, TrFunctionType _ input' output') ->
        input == input' && output == output'
      
      (TrFunction _ output, TrFunction _ output') ->
        output == output'
      
      (TrApplication _ function argument, TrApplication _ function' argument') ->
        function == function' && argument == argument'
      
      (TrSelf _ output, TrSelf _ output') ->
        output == output'
      
      (TrData _ constructor, TrData _ constructor') ->
        constructor == constructor'
      
      (TrCase _ scrutinee, TrCase _ scrutinee') ->
        scrutinee == scrutinee'
      
      (TrPrimitive _ primitive, TrPrimitive _ primitive') ->
        primitive == primitive'
      
      (TrLiteral _ literal, TrLiteral _ literal') ->
        literal == literal'
      
      (TrOperation _ operation arguments, TrOperation _ operation' arguments') ->
        operation == operation' && and (zipWith (==) arguments arguments')

      _ ->
        False

getOrigin :: Term -> Origin
getOrigin term = case term of
  TrReference origin _ -> origin
  TrVariable origin _ -> origin
  TrType origin -> origin
  TrFunctionType origin _ _ -> origin
  TrFunction origin _ -> origin
  TrApplication origin _ _ -> origin
  TrSelf origin _ -> origin
  TrData origin _ -> origin
  TrCase origin _ -> origin
  TrPrimitive origin _ -> origin
  TrLiteral origin _ -> origin
  TrOperation origin _ _ -> origin

shift :: Term -> Term
shift =
  go 0 where
    go depth term =
      case term of
        TrReference origin name ->
          TrReference origin name

        TrVariable origin index ->
          if index >= depth
            then TrVariable origin (succ index)
            else TrVariable origin index

        TrType origin ->
          TrType origin

        TrFunctionType origin input output ->
          TrFunctionType origin (go depth input) (go (succ depth) output)

        TrFunction origin output ->
          TrFunction origin (go (succ depth) output)

        TrApplication origin function argument ->
          TrApplication origin (go depth function) (go depth argument)

        TrSelf origin output ->
          TrSelf origin (go (succ depth) output)
        
        TrData origin constructor ->
          TrData origin (go depth constructor)
        
        TrCase origin scrutinee ->
          TrCase origin (go depth scrutinee)
        
        TrPrimitive origin primitive ->
          TrPrimitive origin primitive
        
        TrLiteral origin literal ->
          TrLiteral origin literal

        TrOperation origin operation arguments ->
          TrOperation origin operation (map (go depth) arguments)

instantiate :: Term -> Term -> Term
instantiate term =
  go 0 where
    go depth scope =
      case scope of
        TrReference origin name ->
          TrReference origin name

        TrVariable origin index ->
          case compare index depth of
            LT -> TrVariable origin index
            EQ -> term
            GT -> TrVariable origin (pred index)

        TrType origin ->
          TrType origin

        TrFunctionType origin input output ->
          TrFunctionType origin (go depth input) (go (succ depth) output)

        TrFunction origin output ->
          TrFunction origin (go (succ depth) output)

        TrApplication origin function argument ->
          TrApplication origin (go depth function) (go depth argument)

        TrSelf origin output ->
          TrSelf origin (go (succ depth) output)
        
        TrData origin constructor ->
          TrData origin (go depth constructor)
        
        TrCase origin scrutinee ->
          TrCase origin (go depth scrutinee)
        
        TrPrimitive origin primitive ->
          TrPrimitive origin primitive
        
        TrLiteral origin literal ->
          TrLiteral origin literal

        TrOperation origin operation arguments ->
          TrOperation origin operation (map (go depth) arguments)

trReference :: Name -> Term
trReference = TrReference Nothing

trVariable :: Index -> Term
trVariable = TrVariable Nothing

trType :: Term
trType = TrType Nothing

trFunctionType :: Type -> Type -> Term
trFunctionType = TrFunctionType Nothing

trFunction :: Term -> Term
trFunction = TrFunction Nothing

trApplication :: Term -> Term -> Term
trApplication = TrApplication Nothing

trSelf :: Type -> Term
trSelf = TrSelf Nothing

trData :: Term -> Term
trData = TrData Nothing

trCase :: Term -> Term
trCase = TrCase Nothing

trPrimitive :: Primitive -> Term
trPrimitive = TrPrimitive Nothing

trLiteral :: Literal -> Term
trLiteral = TrLiteral Nothing

trOperation :: Operation -> [Term] -> Term
trOperation = TrOperation Nothing
