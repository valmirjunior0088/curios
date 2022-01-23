module Curios.Core.Elaboration
  ( evaluate
  , elaborate
  )
  where

import Curios.Core.Term (Name, Term (..))
import qualified Curios.Core.Term as Core
import qualified Curios.Source.Expression as Source

import Curios.Core.Context
  ( Error
  , Context
  , Contextual
  , execContextual
  , evalContextual
  , whnf
  , insertDeclaration
  , insertDefinition
  )

import Curios.Source.Expression
  ( Identifier (..)
  , FunctionTypeBinding (..)
  , FunctionBinding (..)
  , SelfBinding (..)
  , Binding (..)
  , Expression (..)
  , Item (..)
  , Items (..)
  )

abstract :: Name -> Term -> Term
abstract name =
  go 0 where
    go depth term =
      case term of
        TrReference origin name' ->
          if name == name'
            then TrVariable origin depth
            else TrReference origin name'

        TrVariable origin index ->
          TrVariable origin index

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

translate :: Expression -> Term
translate expression =
  case expression of
    ExParentheses _ expression' ->
      translate expression'

    ExType sourcePos ->
      TrType (Just sourcePos)

    ExFunctionType sourcePos (FtDependent _ (Identifier _ name) input) output ->
      TrFunctionType (Just sourcePos)
        (translate input)
        (abstract name $ translate output)
    
    ExFunctionType sourcePos (FtNonDependent _ input) output ->
      TrFunctionType (Just sourcePos)
        (translate input) (translate output)
    
    ExFunction sourcePos (FunctionBinding _ (Identifier _ name)) output ->
      TrFunction (Just sourcePos) (abstract name $ translate output)

    ExApplication sourcePos function arguments ->
      foldl (TrApplication $ Just sourcePos)
        (translate function)
        (map translate arguments)

    ExIdentifier sourcePos (Identifier _ name) ->
      TrReference (Just sourcePos) name

    ExSelf sourcePos (SelfBinding _ (Identifier _ name)) output ->
      TrSelf (Just sourcePos) (abstract name $ translate output)

    ExData sourcePos constructor ->
      TrData (Just sourcePos) (translate constructor)

    ExCase sourcePos scrutinee ->
      TrCase (Just sourcePos) (translate scrutinee)

    ExPrimitive sourcePos primitive ->
      TrPrimitive (Just sourcePos) $ case primitive of
        Source.PrInt32 _ -> Core.PrInt32
        Source.PrFlt32 _ -> Core.PrFlt32

    ExLiteral sourcePos literal ->
      TrLiteral (Just sourcePos) $ case literal of
        Source.LtInt32 _ value -> Core.LtInt32 value
        Source.LtFlt32 _ value -> Core.LtFlt32 value

    ExOperation sourcePos operation arguments ->
      TrOperation (Just sourcePos) operation' (map translate arguments) where
        operation' = case operation of
          Source.OpInt32Sum _ -> Core.OpInt32Sum 
          Source.OpFlt32Sum _ -> Core.OpFlt32Sum 

bindFunctionType :: Binding -> Term -> Term
bindFunctionType (Binding sourcePos (Identifier _ name) expression) term =
  TrFunctionType (Just sourcePos) (translate expression) (abstract name term)

bindFunction :: Binding -> Term -> Term
bindFunction (Binding sourcePos (Identifier _ name) _) term =
  TrFunction (Just sourcePos) (abstract name term)

declare :: Item -> Contextual ()
declare (Item _ (Identifier sourcePos name) bindings expression _) =
  insertDeclaration (Just sourcePos, name) 
    (foldr bindFunctionType (translate expression) bindings)

define :: Item -> Contextual ()
define (Item _ (Identifier sourcePos name) bindings _ expression) =
  insertDefinition (Just sourcePos, name) 
    (foldr bindFunction (translate expression) bindings)

process :: Items -> Contextual ()
process (Items _ items) =
  mapM_ declare items >> mapM_ define items

evaluate :: Items -> Term -> Either Error Term
evaluate items term =
  evalContextual (process items >> whnf term)

elaborate :: Items -> Either Error Context
elaborate items =
  execContextual (process items)
