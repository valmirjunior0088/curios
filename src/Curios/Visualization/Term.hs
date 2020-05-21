module Curios.Visualization.Term
  (prToBox
  ,inToBox
  ,unToBox
  ,scToBox
  ,teToBox
  )
  where

import Prelude hiding
  ((<>)
  )

import Curios.Term
  (Primitive (..)
  ,Index (..)
  ,Universe (..)
  ,Scope (..)
  ,Term (..)
  )

import Curios.Visualization.Expression
  (liToBox
  ,qnToBox
  )

import Curios.Visualization.Identifier
  (idToBox
  )

import Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,roofed
  ,floored
  ,upwardsArrow
  ,downwardsArrow
  )

import Text.PrettyPrint.Boxes
  (Box
  ,char
  ,text
  ,rows
  ,(<+>)
  ,(/+/)
  ,(//)
  )

prToBox :: Primitive -> Box
prToBox primitive =
  text (show primitive)

inToBox :: Index -> Box
inToBox index =
  text (show index)

unToBox :: Universe -> Box
unToBox universe =
  text (show universe)

scToBox :: Scope -> Box
scToBox (Scope body) =
  teToBox body

teToBox :: Term -> Box
teToBox term =
  case term of
    TePrimitive primitive ->
      text "Primitive" <+> parenthesized (prToBox primitive)
    TeLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)
    TeFreeVariable qualifiedName ->
      text "Free variable" <+> parenthesized (qnToBox qualifiedName)
    TeBoundVariable index ->
      text "Bound variable" <+> parenthesized (inToBox index)
    TeMetaVariable identifier variableType ->
      text "Meta variable" <+> parenthesized (idToBox identifier) <+> emphasized (teToBox variableType)
    TeType universe ->
      text "Type" <+> parenthesized (unToBox universe)
    TePiAbstraction variableType scope ->
      let
        content = teToBox variableType /+/ scToBox scope
        body = (downwardsArrow (rows content - 1) // char 'Π') <+> content
      in
        roofed body
    TeLambdaAbstraction variableType scope ->
      let
        content = teToBox variableType /+/ scToBox scope
        body = (downwardsArrow (rows content - 1) // char 'λ') <+> content
      in
        roofed body
    TeApplication function argument ->
      let
        content = teToBox function /+/ teToBox argument
        body = (char '◆' // upwardsArrow (rows content - 1)) <+> content
      in
        floored body
