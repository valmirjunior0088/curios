module Curios.Visualization.Expression
  (liToBox
  ,naToBox
  ,qnToBox
  ,pbToBox
  ,lbToBox
  ,exToBox
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Expression (..)
  )

import Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,upwardsSeparated
  ,downwardsSeparated
  ,upwardsTab
  ,downwardsTab
  )

import Text.PrettyPrint.Boxes
  (Box
  ,text
  ,(<+>)
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
  variableName <+> emphasized variableType where
    variableName = maybe (text "Unnamed") naToBox maybeName
    variableType = exToBox expression

lbToBox :: LambdaBinding -> Box
lbToBox (LambdaBinding name maybeExpression) =
  variableName <+> emphasized variableType where
    variableName = naToBox name
    variableType = maybe (text "Untyped") exToBox maybeExpression

exToBox :: Expression -> Box
exToBox expression =
  case expression of
    ExLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)
    ExVariable qualifiedName ->
      text "Variable" <+> parenthesized (qnToBox qualifiedName)
    ExPiAbstraction piBindings abstractionBody ->
      downwardsTab 'Π' (downwardsSeparated (fmap pbToBox piBindings)) (exToBox abstractionBody)
    ExLambdaAbstraction lambdaBindings abstractionBody ->
      downwardsTab 'λ' (downwardsSeparated (fmap lbToBox lambdaBindings)) (exToBox abstractionBody)
    ExApplication function arguments ->
      upwardsTab '◆' (exToBox function) (upwardsSeparated (fmap exToBox arguments))
