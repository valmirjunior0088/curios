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
import Data.Map (assocs)

import Curios.Core.Term (Name, Index, Literal (..))
import qualified Curios.Core.Term as Core

data Operation =
  OpInt32Sum Term Term |
  OpFlt32Sum Term Term

data Term =
  TrReference Name |
  TrVariable Index |
  TrFunction Term |
  TrApplication Term Term |
  TrLiteral Literal |
  TrOperation Operation |
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

    Core.TrOperation _ operation ->
      TrOperation $ case operation of
        Core.OpInt32Sum one other -> OpInt32Sum (unwrap one) (unwrap other)
        Core.OpFlt32Sum one other -> OpFlt32Sum (unwrap one) (unwrap other)

data Item =
  Item Name Term

process :: (Name, Core.Term) -> Item
process (name, term) =
  Item name (unwrap term)

erase :: Context -> [Item]
erase (_, definitions) =
  fmap process (assocs definitions)
