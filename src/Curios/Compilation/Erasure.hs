module Curios.Compilation.Erasure
  ( Name
  , Index
  , Literal (..)
  , Operation (..)
  , Term (..)
  , Item (..)
  , erase
  )
  where

import Curios.Core.Context (Context)

import Curios.Core.Term (Name, Index, Literal (..), Operation (..))
import qualified Curios.Core.Term as Core

import qualified Data.Map as Map

data Term =
  TrReference Name |
  TrVariable Index |
  TrFunction Term |
  TrApplication Term Term |
  TrLiteral Literal |
  TrOperation Operation [Term] |
  TrNull

unwrap :: Core.Term -> Term
unwrap term =
  case term of
    Core.TrReference _ name ->
      TrReference name

    Core.TrVariable _ index ->
      TrVariable index

    Core.TrType _ ->
      TrNull

    Core.TrFunctionType _ _ _ ->
      TrNull

    Core.TrFunction _ output ->
      TrFunction (unwrap output)
      
    Core.TrApplication _ function argument ->
      TrApplication (unwrap function) (unwrap argument)

    Core.TrSelf _ _ ->
      TrNull

    Core.TrData _ constructor ->
      unwrap constructor

    Core.TrCase _ scrutinee ->
      unwrap scrutinee

    Core.TrPrimitive _ _ ->
      TrNull
    
    Core.TrLiteral _ literal ->
      TrLiteral literal

    Core.TrOperation _ operation arguments ->
      TrOperation operation (map unwrap arguments)

data Item =
  Item Name Term

combine :: Name -> Core.Term -> [Item] -> [Item]
combine name term items =
  Item name (unwrap term) : items

erase :: Context -> Maybe [Item]
erase (_, definitions) =
  if Map.member "main" definitions
    then Just (Map.foldrWithKey combine [] definitions)
    else Nothing
