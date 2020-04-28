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
  header // (char '◥' <+> body) where
    header = text ("PiBinding" ++ (maybe "" (\name -> " (" ++ show name ++ ")") maybeName))
    body = exToBox expression

lbToBox :: LambdaBinding -> Box
lbToBox (LambdaBinding name maybeExpression) =
  maybe header (\body -> header // (char '◥' <+> body)) maybeBody where
    header = text ("LambdaBinding (" ++ show name ++ ")")
    maybeBody = fmap exToBox maybeExpression

abToBox :: (a -> Box) -> Abstraction a -> Box
abToBox toBox (Abstraction bindings expression) =
  let
    trunk size = vcat left (char '┏' : replicate (size - 1) (char '┃'))
    branches = (fmap (\branch -> (trunk (rows branch) <+> branch) // char '▼') . fmap toBox) bindings
    tree = vcat left branches
    root = char '◆' <+> exToBox expression
  in
    tree // root

exToBox :: Expression -> Box
exToBox expression =
  case expression of
    ExLiteral literal ->
      text "ExLiteral (" <> liToBox literal <> text ")"
    ExVariable qualifiedName ->
      text "ExVariable (" <> qnToBox qualifiedName <> text ")"
    ExPiAbstraction abstraction ->
      text "ExPiAbstraction" // abToBox pbToBox abstraction
    ExLambdaAbstraction abstraction ->
      text "ExLambdaAbstraction" // abToBox lbToBox abstraction
    ExApplication function arguments ->
      let
        trunk size = vcat left (replicate (size - 1) (char '┃') ++ [char '┗'])
        branches = (fmap (\branch -> char '▲' // (trunk (rows branch) <+> branch)) . fmap exToBox) arguments
        tree = vcat left branches
        root = char '◆' <+> exToBox function
      in
        text "ExApplication" // root // tree