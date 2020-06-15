module Curios.Visualization.Term
  (prToBox
  ,ntToBox
  ,unToBox
  ,ssToBox
  ,teToBox
  )
  where

import Curios.Visualization.Expression
  (naToBox
  ,liToBox
  )

import Curios.Term
  (Primitive (..)
  ,Scope (..)
  ,Term (..)
  )

import Curios.Universe
  (Universe (..)
  )

import Curios.Visualization.Common
  (parenthesized
  ,upwardsTab
  ,downwardsTab
  )

import GHC.Natural
  (Natural (..)
  )

import Text.PrettyPrint.Boxes
  (Box
  ,text
  ,(<+>)
  )

prToBox :: Primitive -> Box
prToBox primitive =
  text (show primitive)

ntToBox :: Natural -> Box
ntToBox natural =
  text (show natural)

unToBox :: Universe -> Box
unToBox universe =
  text (show universe)

ssToBox :: Scope -> Box
ssToBox (Scope output) =
  teToBox output

teToBox :: Term -> Box
teToBox term =
  case term of
    TrPrimitive primitive ->
      text "Primitive" <+> parenthesized (prToBox primitive)

    TrLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)

    TrFreeVariable name ->
      text "Free variable" <+> parenthesized (naToBox name)

    TrBoundVariable index ->
      text "Bound variable" <+> parenthesized (ntToBox index)
      
    TrType universe ->
      text "Type" <+> parenthesized (unToBox universe)

    TrAbstractionType variableType scope ->
      downwardsTab 'Π' (teToBox variableType) (ssToBox scope)

    TrAbstraction variableType scope ->
      downwardsTab 'λ' (teToBox variableType) (ssToBox scope)
      
    TrApplication function argument ->
      upwardsTab '◆' (teToBox function) (teToBox argument)
