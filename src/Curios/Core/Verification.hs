module Curios.Core.Verification
  (trReduce
  ,trConvertsWith
  ,trCheck
  )
  where

import Curios.Core.Declarations (Declarations (..), dcLookup)
import Curios.Core.Definitions (Definitions (..), dfEmpty, dfLookup)
import Curios.Core.History (History, hsEmpty, hsInsert, hsAny)
import Curios.Core.Variables (Variables (..), vrEmpty, vrAllocate, vrLookup)
import Control.Monad (unless)
import Data.Maybe (fromJust)

import Curios.Error
  (Error (..)
  ,erMismatchedFunctionType
  ,erMismatchedType
  ,erUndeclaredName
  ,erNonInferable
  )

import Curios.Core.Term
  (Argument (..)
  ,Primitive (..)
  ,Literal (..)
  ,Operator (..)
  ,Type
  ,Depth
  ,Term (..)
  ,trType
  ,trPrimitive
  ,trOrigin
  )

trReduce :: Definitions -> Term -> Term
trReduce definitions term =
  case term of
    TrReference origin name -> 
      case dfLookup name definitions of
        Just term' -> trReduce definitions term'
        Nothing -> TrReference origin name
    TrApplication origin function argument ->
      case (trReduce definitions function, trReduce definitions argument) of
        (TrOperator _ _ (OpUnary operator), TrLiteral _ literal) ->
          trReduce definitions (operator literal)
        (TrApplication _ (TrOperator _ _ (OpBinary operator)) (TrLiteral _ one), TrLiteral _ another) ->
          trReduce definitions (operator one another)
        (TrFunction _ output, argument') ->
          trReduce definitions (output (ArTerm argument'))
        (function', argument') ->
          TrApplication origin function' argument'
    term' ->
      term'

trConvertsWith :: Definitions -> Term -> Term -> Bool
trConvertsWith definitions =
  eqrec hsEmpty 0 where
    
    alpha :: Depth -> Term -> Term -> Bool
    alpha depth one other =
      case (one, other) of
        (TrPrimitive _ primitive, TrPrimitive _ primitive') ->
          primitive == primitive'
        (TrLiteral _ literal, TrLiteral _ literal') ->
          literal == literal'
        (TrOperator _ name _, TrOperator _ name' _) ->
          name == name'
        (TrReference _ name, TrReference _ name') ->
          name == name'
        (TrVariable _ index, TrVariable _ index') ->
          index == index'
        (TrType _, TrType _) ->
          True
        (TrFunctionType _ input output, TrFunctionType _ input' output') ->
          (&&)
            (alpha depth input input')
            (alpha (succ (succ depth))
              (output (ArPlaceholder depth) (ArPlaceholder (succ depth)))
              (output' (ArPlaceholder depth) (ArPlaceholder (succ depth)))
            )
        (TrFunction _ output, TrFunction _ output') ->
          alpha (succ depth)
            (output (ArPlaceholder depth))
            (output' (ArPlaceholder depth))
        (TrApplication _ function argument, TrApplication _ function' argument') ->
          (&&)
            (alpha depth function function')
            (alpha depth argument argument')
        _ ->
          False
    
    beta :: Depth -> Term -> Term -> Bool
    beta depth one other =
      alpha depth (trReduce dfEmpty one) (trReduce dfEmpty other)
    
    predicate :: Depth -> (Term, Term) -> (Term, Term) -> Bool
    predicate depth (one, other) (one', other') =
      alpha depth one one' && alpha depth other other'
    
    eqrec :: History -> Depth -> Term -> Term -> Bool
    eqrec history depth one other =
      beta depth one other || hsAny (predicate depth (one, other)) history || comparison where
        history' = hsInsert (one, other) history
        comparison =
          case (trReduce definitions one, trReduce definitions other) of
            (TrFunctionType _ input output, TrFunctionType _ input' output') ->
              (&&)
                (eqrec history' depth input input')
                (eqrec history' (succ (succ depth))
                  (output (ArPlaceholder depth) (ArPlaceholder (succ depth)))
                  (output' (ArPlaceholder depth) (ArPlaceholder (succ depth)))
                )
            (TrFunction _ output, TrFunction _ output') ->
              eqrec history' (succ depth)
                (output (ArPlaceholder depth))
                (output' (ArPlaceholder depth))
            (TrApplication _ function argument, TrApplication _ function' argument') ->
              (&&)
                (eqrec history' depth function function')
                (eqrec history' depth argument argument')
            (one', other') ->
              alpha depth one' other'

trCheck :: Declarations -> Definitions -> Type -> Term -> Either Error ()
trCheck declarations definitions =
  check vrEmpty where

    check :: Variables -> Type -> Term -> Either Error ()
    check variables termType term =
      case (trReduce definitions termType, term) of
        (TrFunctionType _ input output, TrFunction _ output') ->
          check variables' (output (ArTerm term) variableArgument) (output' variableArgument) where
            (variableArgument, variables') = vrAllocate input variables
        (termType', TrFunction origin _) ->
          Left (erMismatchedFunctionType origin termType')
        (termType', term') -> do
          termType'' <- infer variables term'

          unless
            (trConvertsWith definitions termType' termType'')
            (Left (erMismatchedType (trOrigin term') termType termType''))
          
          Right ()
    
    infer :: Variables -> Term -> Either Error Type
    infer variables term =
      case term of
        TrPrimitive _ _ ->
          Right trType
        TrLiteral _ literal ->
          Right (trPrimitive primitive) where
            primitive =
              case literal of
                LtText _ -> PrText
                LtInteger _ -> PrInteger
                LtReal _ -> PrReal
        TrOperator _ name _ ->
          Right (fromJust (dcLookup name declarations))
        TrVariable _ index ->
          Right (fromJust (vrLookup index variables))
        TrReference origin name ->
          case dcLookup name declarations of
            Nothing -> Left (erUndeclaredName origin name)
            Just termType -> Right termType
        TrType _ ->
          Right trType
        TrFunctionType _ input output -> do
          check variables trType input
          
          let (selfArgument, variables') = vrAllocate term variables
          let (variableArgument, variables'') = vrAllocate input variables'
          check variables'' trType (output selfArgument variableArgument)
          
          Right trType
        TrFunction origin _ ->
          Left (erNonInferable origin)
        TrApplication _ function argument -> do
          functionType <- infer variables function
          
          case trReduce definitions functionType of
            TrFunctionType _ input output -> do
              check variables input argument

              Right (output (ArTerm function) (ArTerm argument))
            functionType' ->
              Left (erMismatchedFunctionType (trOrigin function) functionType')
