module Curios.Visualization.Expression
  (liToBox
  ,idToBox
  ,bnToBox
  ,exToBox
  )
  where

import Curios.Expression
  (Literal (..)
  ,Identifier
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

idToBox :: Identifier -> Box
idToBox identifier =
  text (show identifier)

bnToBox :: Binding -> Box
bnToBox (Binding name expression) =
  (idToBox name) <+> emphasized (exToBox expression) where

exToBox :: Expression -> Box
exToBox expression =
  case expression of
    ExLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)

    ExIdentifier identifier ->
      text "Identifier" <+> parenthesized (idToBox identifier)

    ExAbstractionType bindings body ->
      downwardsTab 'Π' (downwardsSeparated (fmap bnToBox bindings)) (exToBox body)

    ExAbstraction bindings body ->
      downwardsTab 'λ' (downwardsSeparated (fmap bnToBox bindings)) (exToBox body)
      
    ExApplication function arguments ->
      upwardsTab '◆' (exToBox function) (upwardsSeparated (fmap exToBox arguments))
