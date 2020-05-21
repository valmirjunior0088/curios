module Curios.Visualization.Expression
  (liToBox
  ,naToBox
  ,qnToBox
  ,pbToBox
  ,lbToBox
  ,abToBox
  ,exToBox
  )
  where

import Prelude hiding
  ((<>)
  )

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Abstraction (..)
  ,Expression (..)
  )

import Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,newlined
  ,horizontalLine
  ,floored
  ,roofed
  ,upwardsArrow
  ,downwardsArrow
  )

import Text.PrettyPrint.Boxes
  (Box
  ,emptyBox
  ,char
  ,text
  ,vcat
  ,left
  ,rows
  ,cols
  ,(<>)
  ,(<+>)
  ,(//)
  )

liToBox :: Literal -> Box
liToBox literal =
  text (show literal)

naToBox :: Name -> Box
naToBox name =
  text (show name)

qnToBox :: QualifiedName -> Box
qnToBox qualifiedName =
  text (show qualifiedName)

pbToBox :: PiBinding -> Box
pbToBox (PiBinding maybeName expression) =
  roofed (downwardsArrow (rows body) <+> body) where
    variableName = maybe (text "Unnamed") naToBox maybeName
    variableType = exToBox expression
    body = newlined (variableName <+> emphasized variableType)

lbToBox :: LambdaBinding -> Box
lbToBox (LambdaBinding name maybeExpression) =
  roofed (downwardsArrow (rows body) <+> body) where
    variableName = naToBox name
    variableType = maybe (text "Untyped") exToBox maybeExpression
    body = newlined (variableName <+> emphasized variableType)

abToBox :: Char -> (a -> Box) -> Abstraction a -> Box
abToBox symbol toBox (Abstraction bindings expression) =
  bindings' // (char symbol <+> horizontalLine (cols expression' - 2)) // expression' where
    bindings' = vcat left (fmap toBox bindings)
    expression' = exToBox expression

exToBox :: Expression -> Box
exToBox expression =
  case expression of
    ExLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)
    ExVariable qualifiedName ->
      text "Variable" <+> parenthesized (qnToBox qualifiedName)
    ExPiAbstraction piAbstraction ->
      abToBox 'Π' pbToBox piAbstraction
    ExLambdaAbstraction lambdaAbstraction ->
      abToBox 'λ' lbToBox lambdaAbstraction
    ExApplication function arguments ->
      let
        function' = exToBox function
        arguments' = vcat left ((fmap (\argument' -> floored (upwardsArrow (rows argument') <+> argument')) . fmap exToBox) arguments)
      in
        function' // (char '◆' <+> horizontalLine (cols function' - 2)) // arguments'