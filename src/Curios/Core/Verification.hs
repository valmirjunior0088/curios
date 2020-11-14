module Curios.Core.Verification
  (trReduce
  ,trConvertsWith
  ,trCheck
  )
  where

import Data.Maybe (fromJust)
import Control.Monad (unless)
import Curios.Error (Error (..))
import Curios.Syntax.Expression (Primitive (..))
import Curios.Core.Term (PrimitiveType (..), Type, Term (..))
import Curios.Core.Seen (snEmpty, snMember, snInsert)
import Curios.Core.Bindings (Bindings (..), bnLookup)
import Curios.Core.Definitions (Definitions (..), dfLookup)
import Curios.Core.Environment (Environment (..), enEmpty, enInsert, enLookup, enLength)

trReduce :: Definitions -> Term -> Term
trReduce definitions term =
  case term of
    TrReference name -> 
      case dfLookup name definitions of
        Nothing -> TrReference name
        Just term' -> trReduce definitions term'
    TrApplication function argument ->  
      case trReduce definitions function of
        TrFunction output -> trReduce definitions (output argument)
        function' -> function'
    term' ->
      term'

trConvertsWith :: Definitions -> Term -> Term -> Bool
trConvertsWith definitions =
  go snEmpty 0 where
    go seen depth one other =
      snMember (one', other') seen || comparison where
        one' = trReduce definitions one
        other' = trReduce definitions other
        seen' = snInsert (one', other') seen
        comparison =
          case (one', other') of
            (TrFunctionType input output, TrFunctionType input' output') ->
              (&&)
                (go seen' (depth + 0) input input')
                (go seen' (depth + 2)
                  (output (TrVariable (depth + 0)) (TrVariable (depth + 1)))
                  (output' (TrVariable (depth + 0)) (TrVariable (depth + 1)))
                )
            (TrFunction output, TrFunction output') ->
              go seen' (depth + 1)
                (output (TrVariable (depth + 0)))
                (output' (TrVariable (depth + 0)))
            (TrApplication function argument, TrApplication function' argument') ->
              (&&)
                (go seen' (depth + 0) function function')
                (go seen' (depth + 0) argument argument')
            (TrAnnotated termType term, TrAnnotated termType' term') ->
              (&&)
                (go seen' (depth + 0) termType termType')
                (go seen' (depth + 0) term term')
            (one'', other'') ->
              one'' == other''

trCheck :: Bindings -> Definitions -> Type -> Term -> Either Error ()
trCheck bindings definitions =
  check enEmpty where

    check :: Environment -> Type -> Term -> Either Error ()
    check environment termType term =
      case (trReduce definitions termType, term) of
        (TrFunctionType input output, TrFunction output') ->
          let
            self = TrAnnotated termType term
            variable = TrVariable (enLength environment)
            environment' = enInsert input environment
          in
            check environment' (output self variable) (output' variable)
        (termType', term') ->
          do
            termType'' <- infer environment term'

            unless
              (trConvertsWith definitions termType' termType'')
              (Left (ErIllTypedTerm environment termType' term'))
            
            Right ()
    
    infer :: Environment -> Term -> Either Error Type
    infer environment term =
      case term of
        TrVariable index ->
          Right (fromJust (enLookup index environment))
        TrReference name ->
          bnLookup name bindings
        TrType ->
          Right TrType
        TrPrimitiveType _ ->
          Right TrType
        TrPrimitive primitive ->
          Right (TrPrimitiveType primitiveType) where
            primitiveType =
              case primitive of
                PrText _ -> PtText
                PrInteger _ -> PtInteger
                PrRational _ -> PtRational
        TrFunctionType input output ->
          do
            check environment TrType input
            
            let self = TrVariable (enLength environment)
            let environment' = enInsert term environment
            let variable = TrVariable (enLength environment')
            let environment'' = enInsert input environment'
            check environment'' TrType (output self variable)
            
            Right TrType
        TrFunction output ->
          Left (ErNonInferableTerm (TrFunction output))
        TrApplication function argument ->
          do
            functionType <- infer environment function
            
            case functionType of
              TrFunctionType input output ->
                do
                  check environment input argument

                  Right (output function argument)
              _ ->
                Left (ErNonFunctionApplication (TrApplication function argument))
        TrAnnotated termType term' ->
          do
            check environment TrType termType
            check environment termType term'
            
            Right termType