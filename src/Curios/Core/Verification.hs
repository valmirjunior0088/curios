module Curios.Core.Verification
  (trReduce
  ,trConvertsWith
  ,trCheck
  )
  where

import Curios.Core.History (hsEmpty, hsInsert, hsMember)
import Curios.Core.Declarations (Declarations (..), dcLookup)
import Curios.Core.Definitions (Definitions (..), dfLookup)
import Curios.Core.Variables (Variables (..), vrEmpty, vrInsert, vrLookup, vrNext)
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
  (Origin (..)
  ,Argument (..)
  ,Primitive (..)
  ,Literal (..)
  ,Operator (..)
  ,Type
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
    TrAnnotated _ _ term' ->
      trReduce definitions term'
    term' ->
      term'

trConvertsWith :: Definitions -> Term -> Term -> Bool
trConvertsWith definitions =
  go hsEmpty 0 where
    go history depth one other =
      hsMember (one', other') history || comparison where
        one' = trReduce definitions one
        other' = trReduce definitions other
        history' = hsInsert (one', other') history
        comparison =
          case (one', other') of
            (TrFunctionType _ input output, TrFunctionType _ input' output') ->
              (&&)
                (go history' (depth + 0) input input')
                (go history' (depth + 2)
                  (output (ArPlaceholder (depth + 0)) (ArPlaceholder (depth + 1)))
                  (output' (ArPlaceholder (depth + 0)) (ArPlaceholder (depth + 1)))
                )
            (TrFunction _ output, TrFunction _ output') ->
              go history' (depth + 1)
                (output (ArPlaceholder (depth + 0)))
                (output' (ArPlaceholder (depth + 0)))
            (TrApplication _ function argument, TrApplication _ function' argument') ->
              (&&)
                (go history' (depth + 0) function function')
                (go history' (depth + 0) argument argument')
            (TrAnnotated _ termType term, TrAnnotated _ termType' term') ->
              (&&)
                (go history' (depth + 0) termType termType')
                (go history' (depth + 0) term term')
            (one'', other'') ->
              one'' == other''

trCheck :: Declarations -> Definitions -> Type -> Term -> Either Error ()
trCheck declarations definitions =
  check vrEmpty where

    check :: Variables -> Type -> Term -> Either Error ()
    check variables termType term =
      case (trReduce definitions termType, term) of
        (TrFunctionType _ input output, TrFunction _ output') ->
          let
            selfArgument = ArTerm (TrAnnotated OrMachine termType term)
            variableArgument = ArPlaceholder (vrNext variables)
            variables' = vrInsert input variables
          in
            check variables' (output selfArgument variableArgument) (output' variableArgument)
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
          
          let selfArgument = ArPlaceholder (vrNext variables)
          let variables' = vrInsert term variables
          let variableArgument = ArPlaceholder (vrNext variables')
          let variables'' = vrInsert input variables'
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
        TrAnnotated _ termType term' -> do
          check variables trType termType
          check variables termType term'
          
          Right termType
