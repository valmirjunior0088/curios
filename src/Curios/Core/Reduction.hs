module Curios.Core.Reduction
  ( trReduce
  )
  where

import Curios.Core (Variable (..), Operator (..), Term (..))
import Curios.Core.Definitions (Definitions, dfLookup)

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
