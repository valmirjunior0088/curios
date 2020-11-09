module Curios.Core.Verification
  (trReduce
  ,trConvertsWith
  ,trInfer
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

trInfer' :: Environment -> Bindings -> Definitions -> Term -> Either Error Type
trInfer' environment bindings definitions term =
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
        trCheck' environment bindings definitions TrType input
        
        let self = TrVariable (enLength environment)
        let environment' = enInsert term environment
        let variable = TrVariable (enLength environment')
        let environment'' = enInsert input environment'
        trCheck' environment'' bindings definitions TrType (output self variable)
        
        Right TrType
    TrFunction output ->
      Left (ErNonInferableTerm (TrFunction output))
    TrApplication function argument ->
      do
        functionType <- trInfer' environment bindings definitions function
        
        case functionType of
          TrFunctionType input output ->
            do
              trCheck' environment bindings definitions input argument
              
              Right (output function argument)
          _ ->
            Left (ErNonFunctionApplication (TrApplication function argument))
    TrAnnotated termType term' ->
      do
        trCheck' environment bindings definitions TrType termType
        trCheck' environment bindings definitions termType term'

        Right termType

trCheck' :: Environment -> Bindings -> Definitions -> Type -> Term -> Either Error ()
trCheck' environment bindings definitions termType term =
  case (trReduce definitions termType, term) of
    (TrFunctionType input output, TrFunction output') ->
      let
        self = TrAnnotated termType term
        variable = TrVariable (enLength environment)
        environment' = enInsert input environment
      in
        trCheck' environment' bindings definitions (output self variable) (output' variable)
    (termType', term') ->
      do
        termType'' <- trInfer' environment bindings definitions term

        unless
          (trConvertsWith definitions termType' termType'')
          (Left (ErIllTypedTerm environment termType' term'))
        
        Right ()

trInfer :: Bindings -> Definitions -> Term -> Either Error Type
trInfer =
  trInfer' enEmpty

trCheck :: Bindings -> Definitions -> Type -> Term -> Either Error ()
trCheck =
  trCheck' enEmpty
