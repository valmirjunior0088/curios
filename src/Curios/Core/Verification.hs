module Curios.Core.Verification
  ( trCheck
  )
  where

import Curios.Core.Declarations (Declarations, dcLookup)
import Curios.Core.Definitions (Definitions)
import Curios.Core.Variables (Variables, vrEmpty, vrAllocate, vrLookup, vrDepth)
import Curios.Core.Reduction (trReduce)
import Curios.Core.Conversion (trConverts)
import Curios.Core.Error (Error (..), Kind (..), throw)
import Data.Maybe (fromJust)
import Control.Monad (unless)

import Curios.Core
  ( Literal (..)
  , Variable (..)
  , Type
  , Term (..)
  , trType
  , trPrText
  , trPrInteger 
  , trPrReal 
  , trOrigin
  )

ltInfer :: Literal -> Type
ltInfer literal =
  case literal of
    LtText _ -> trPrText
    LtInteger _ -> trPrInteger
    LtReal _ -> trPrReal

trCheck :: Declarations -> Definitions -> Type -> Term -> Either Error ()
trCheck declarations definitions =
  check vrEmpty where
    
    infer :: Variables -> Term -> Either Error Type
    infer variables term =
      case term of
        TrPrimitive _ _ ->
          return trType

        TrLiteral _ literal ->
          return (ltInfer literal)

        TrOperator _ name _ ->
          return (fromJust (dcLookup name declarations))

        TrVariable _ index ->
          return (fromJust (vrLookup index variables))

        TrReference origin name ->
          case dcLookup name declarations of
            Nothing -> throw origin variables (KnUndeclaredName name)
            Just termType -> return termType

        TrType _ ->
          return trType

        TrFunctionType _ inputType output -> do
          check variables trType inputType
          
          let
            (self, variables') = vrAllocate term variables
            (input, variables'') = vrAllocate inputType variables'
            
          check variables'' trType (output self input)
          
          return trType

        TrFunction origin _ ->
          throw origin variables KnNonInferable

        TrApplication _ function argument -> do
          functionType <- infer variables function
          
          case trReduce definitions functionType of
            TrFunctionType _ inputType output -> do
              check variables inputType argument

              return (output (VrTerm function) (VrTerm argument))

            functionType' ->
              throw (trOrigin function) variables (KnMismatchedFunctionType functionType')
    
    check :: Variables -> Type -> Term -> Either Error ()
    check variables termType term =
      case (trReduce definitions termType, term) of
        (TrFunctionType _ inputType output, TrFunction _ output') ->
          check variables' (output (VrTerm term) input) (output' input) where
            (input, variables') = vrAllocate inputType variables

        (termType', TrFunction origin _) ->
          throw origin variables (KnMismatchedFunctionType termType')

        (termType', term') -> do
          termType'' <- infer variables term'

          unless (trConverts definitions (vrDepth variables) termType' termType'')
            (throw (trOrigin term') variables (KnMismatchedType termType termType''))
          
          return ()
