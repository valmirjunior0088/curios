module Curios.Visualization.Expression
  (liToBox
  ,naToBox
  ,biToBox
  ,exToBox
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,Binding (..)
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

biToBox :: Binding -> Box
biToBox (Binding name expression) =
  (naToBox name) <+> emphasized (exToBox expression) where

exToBox :: Expression -> Box
exToBox expression =
  case expression of
    ExLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)

    ExVariable name ->
      text "Variable" <+> parenthesized (naToBox name)

    ExPiAbstraction bindings body ->
      downwardsTab 'Π' (downwardsSeparated (fmap biToBox bindings)) (exToBox body)

    ExLambdaAbstraction bindings body ->
      downwardsTab 'λ' (downwardsSeparated (fmap biToBox bindings)) (exToBox body)
      
    ExApplication function arguments ->
      upwardsTab '◆' (exToBox function) (upwardsSeparated (fmap exToBox arguments))
