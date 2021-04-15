module Curios.Core.Conversion
  (trConverts
  )
  where

import Curios.Core (Variable (..), Term (..))
import Curios.Core.Definitions (Definitions)
import Curios.Core.Reduction (trReduce)
import GHC.Natural (Natural)

trCongruent :: Natural -> Term -> Term -> Bool
trCongruent depth one other =
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
      trCongruent depth inputType inputType'
        && trCongruent depth' (output self input) (output' self input)
      where
        self = VrQuote depth
        input = VrQuote (succ depth)
        depth' = succ (succ depth)

    (TrFunction _ output, TrFunction _ output') ->
      trCongruent depth' (output input) (output' input) where
        input = VrQuote depth
        depth' = succ depth

    (TrApplication _ function argument, TrApplication _ function' argument') ->
      trCongruent depth function function' && trCongruent depth argument argument'

    _ ->
      False

trBfsConverts :: Definitions -> [(Term, Term)] -> [(Natural, Term, Term)] -> Bool
trBfsConverts definitions history children =
  case children of
    [] ->
      True
      
    ((depth, one, other) : rest) ->
      let
        one' = trReduce definitions one
        other' = trReduce definitions other
        congruent = trCongruent depth one' other'
        predicate (one'', other'') = trCongruent depth one' one'' && trCongruent depth other' other''
        seen = any predicate history
      in
        if congruent || seen
          then trBfsConverts definitions history rest
          else let history' = (one', other') : history in case (one', other') of
            (TrFunctionType _ inputType output, TrFunctionType _ inputType' output') ->
              trBfsConverts definitions history' (rest ++ [inputTypeEquation, outputEquation]) where
                inputTypeEquation = (depth, inputType, inputType')
                self = VrQuote depth
                input = VrQuote (succ depth)
                depth' = succ (succ depth)
                outputEquation = (depth', output self input, output' self input)

            (TrFunction _ output, TrFunction _ output') ->
              trBfsConverts definitions history' (rest ++ [outputEquation]) where
                input = VrQuote depth
                depth' = succ depth
                outputEquation = (depth', output input, output' input)

            (TrApplication _ function argument, TrApplication _ function' argument') ->
              trBfsConverts definitions history' (rest ++ [functionEquation, argumentEquation]) where
                functionEquation = (depth, function, function')
                argumentEquation = (depth, argument, argument')

            _ ->
              False

trConverts :: Definitions -> Natural -> Term -> Term -> Bool
trConverts definitions depth one other =
  trBfsConverts definitions [] [(depth, one, other)]
