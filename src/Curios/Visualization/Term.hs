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
  ,Index
  ,Universe
  ,Scope (..)
  ,Term (..)
  )

import Curios.Visualization.Expression
  (liToBox
  ,qnToBox
  )

import Text.PrettyPrint.Boxes
  (Box
  ,char
  ,text
  ,vcat
  ,left
  ,rows
  ,(<>)
  ,(<+>)
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
  char '◆' <+> teToBox body

teToBox :: Term -> Box
teToBox term =
  case term of
    TePrimitive primitive ->
      text "TePrimitive (" <> prToBox primitive <> text ")"
    TeLiteral literal ->
      text "TeLiteral (" <> liToBox literal <> text ")"
    TeFreeVariable qualifiedName ->
      text "TeFreeVariable (" <> qnToBox qualifiedName <> text ")"
    TeBoundVariable index ->
      text "TeBoundVariable (" <> inToBox index <> text ")"
    TeMetaVariable _ variableType ->
      text "TeMetaVariable (?)" // (char '◥' <+> teToBox variableType)
    TeType universe ->
      text "TeType (" <> unToBox universe <> text ")"
    TePiAbstraction variableType scope ->
      let
        trunk size = vcat left (char '┏' : replicate (size - 1) (char '┃'))
        branch = teToBox variableType
        tree = (trunk (rows branch) <+> branch) // char '▼'
        root = scToBox scope
      in
        text "TePiAbstraction" // tree // root
    TeLambdaAbstraction variableType scope ->
      let
        trunk size = vcat left (char '┏' : replicate (size - 1) (char '┃'))
        branch = teToBox variableType
        tree = (trunk (rows branch) <+> branch) // char '▼'
        root = scToBox scope
      in
        text "TeLambdaAbstraction" // tree // root
    TeApplication function argument ->
      let
        trunk size = vcat left (replicate (size - 1) (char '┃') ++ [char '┗'])
        branch = teToBox argument
        tree = char '▲' // (trunk (rows branch) <+> branch)
        root = char '◆' <+> teToBox function
      in
        text "TeApplication" // root // tree
