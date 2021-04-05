module Curios.Core.Verification
  (trReduce
  ,trConvertsWith
  ,trCheck
  )
  where

import Curios.Core.Declarations (Declarations, dcLookup)
import Curios.Core.Definitions (Definitions, dfEmpty, dfLookup)
import Curios.Core.History (Equation, History, hsEmpty, hsInsert, hsAny)
import Curios.Core.Variables (Variables, vrEmpty, vrAllocate, vrLookup)
import Curios.Core.TypeError (Kind (..), TypeError (..))
import Control.Monad (unless)
import Data.Maybe (fromJust)

import Curios.Core
  (Literal (..)
  ,Operator (..)
  ,Type
  ,Variable (..)
  ,Term (..)
  ,Depth
  ,trPrText
  ,trPrInteger
  ,trPrReal
  ,trType
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
        (TrFunction _ function', argument') ->
          trReduce definitions (function' (VrTerm argument'))
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
        (TrFunctionType _ inputType output, TrFunctionType _ inputType' output') ->
          (&&)
            (alpha depth inputType inputType')
            (alpha (succ (succ depth))
              (output (VrQuote depth) (VrQuote (succ depth)))
              (output' (VrQuote depth) (VrQuote (succ depth)))
            )
        (TrFunction _ output, TrFunction _ output') ->
          alpha (succ depth)
            (output (VrQuote depth))
            (output' (VrQuote depth))
        (TrApplication _ function argument, TrApplication _ function' argument') ->
          (&&)
            (alpha depth function function')
            (alpha depth argument argument')
        _ ->
          False
    
    beta :: Depth -> Term -> Term -> Bool
    beta depth one other =
      alpha depth (trReduce dfEmpty one) (trReduce dfEmpty other)
    
    predicate :: Depth -> Equation -> Equation -> Bool
    predicate depth (one, other) (one', other') =
      alpha depth one one' && alpha depth other other'
    
    seen :: Depth -> Equation -> History -> Bool
    seen depth equation history =
      hsAny (predicate depth equation) history
    
    eqrec :: History -> Depth -> Term -> Term -> Bool
    eqrec history depth one other =
      beta depth one other || seen depth equation history || comparison where
        equation = (one, other)
        history' = hsInsert equation history
        comparison =
          case (trReduce definitions one, trReduce definitions other) of
            (TrFunctionType _ inputType output, TrFunctionType _ inputType' output') ->
              (&&)
                (eqrec history' depth inputType inputType')
                (eqrec history' (succ (succ depth))
                  (output (VrQuote depth) (VrQuote (succ depth)))
                  (output' (VrQuote depth) (VrQuote (succ depth)))
                )
            (TrFunction _ output, TrFunction _ output') ->
              eqrec history' (succ depth)
                (output (VrQuote depth))
                (output' (VrQuote depth))
            (TrApplication _ function argument, TrApplication _ function' argument') ->
              (&&)
                (eqrec history' depth function function')
                (eqrec history' depth argument argument')
            (one', other') ->
              alpha depth one' other'

trCheck :: Declarations -> Definitions -> Type -> Term -> Either TypeError ()
trCheck declarations definitions =
  check vrEmpty where

    check :: Variables -> Type -> Term -> Either TypeError ()
    check variables termType term =
      case (trReduce definitions termType, term) of
        (TrFunctionType _ inputType output, TrFunction _ output') ->
          check variables' (output (VrTerm term) input) (output' input) where
            (input, variables') = vrAllocate inputType variables
        (termType', TrFunction origin _) ->
          Left (TypeError { teOrigin = origin, teKind = KnMismatchedFunctionType termType' })
        (termType', term') -> do
          termType'' <- infer variables term'

          unless
            (trConvertsWith definitions termType' termType'')
            (Left (TypeError { teOrigin = trOrigin term', teKind = KnMismatchedType termType termType' }))
          
          Right ()
    
    infer :: Variables -> Term -> Either TypeError Type
    infer variables term =
      case term of
        TrPrimitive _ _ ->
          Right trType
        TrLiteral _ literal ->
          Right primitive where
            primitive =
              case literal of
                LtText _ -> trPrText
                LtInteger _ -> trPrInteger
                LtReal _ -> trPrReal
        TrOperator _ name _ ->
          Right (fromJust (dcLookup name declarations))
        TrVariable _ index ->
          Right (fromJust (vrLookup index variables))
        TrReference origin name ->
          case dcLookup name declarations of
            Nothing -> Left (TypeError { teOrigin = origin, teKind = KnUndeclaredName name })
            Just termType -> Right termType
        TrType _ ->
          Right trType
        TrFunctionType _ inputType output -> do
          check variables trType inputType
          
          let (self, variables') = vrAllocate term variables
          let (input, variables'') = vrAllocate inputType variables'
          check variables'' trType (output self input)
          
          Right trType
        TrFunction origin _ ->
          Left (TypeError { teOrigin = origin, teKind = KnNonInferable })
        TrApplication _ function argument -> do
          functionType <- infer variables function
          
          case trReduce definitions functionType of
            TrFunctionType _ inputType output -> do
              check variables inputType argument

              Right (output (VrTerm function) (VrTerm argument))
            functionType' ->
              Left (TypeError { teOrigin = trOrigin function, teKind = KnMismatchedFunctionType functionType' })
