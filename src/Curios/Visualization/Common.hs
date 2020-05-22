module Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,horizontalLine
  ,separator
  ,upwardsSeparator
  ,downwardsSeparator
  ,upwardsSeparated
  ,downwardsSeparated
  ,verticalLine
  ,upwardsArrow
  ,downwardsArrow
  ,upwardsTab
  ,downwardsTab
  )
  where

import Prelude hiding
  ((<>)
  )

import Text.PrettyPrint.Boxes
  (Box
  ,nullBox
  ,char
  ,hcat
  ,vcat
  ,punctuateV
  ,left
  ,rows
  ,cols
  ,(<>)
  ,(<+>)
  ,(//)
  )

parenthesized :: Box -> Box
parenthesized box =
  char '(' <> box <> char ')'

emphasized :: Box -> Box
emphasized box =
  char '▶' <+> box

horizontalLine :: Int -> Box
horizontalLine size =
  hcat left (replicate size (char '━'))

separator :: Char -> Int -> Box
separator symbol size =
  if size <= 0 then nullBox else char symbol <> horizontalLine (size - 1)

upwardsSeparator :: Int -> Box
upwardsSeparator size =
  separator '▲' size

downwardsSeparator :: Int -> Box
downwardsSeparator size =
  separator '▼' size

maximumWidth :: Foldable f => f Box -> Int
maximumWidth boxes =
  foldl (\accumulator box -> max accumulator (cols box)) 0 boxes

upwardsSeparated :: Foldable f => f Box -> Box
upwardsSeparated boxes =
  punctuateV left (upwardsSeparator (maximumWidth boxes)) boxes

downwardsSeparated :: Foldable f => f Box -> Box
downwardsSeparated boxes =
  punctuateV left (downwardsSeparator (maximumWidth boxes)) boxes

verticalLine :: Int -> Box
verticalLine size =
  vcat left (replicate size (char '┃'))

upwardsArrow :: Int -> Box
upwardsArrow size =
  if size <= 0 then nullBox else char '▲' // verticalLine (size - 1)

downwardsArrow :: Int -> Box
downwardsArrow size =
  if size <= 0 then nullBox else verticalLine (size - 1) // char '▼'

upwardsTab :: Char -> Box -> Box -> Box
upwardsTab symbol content label =
  body // upwardsSeparator (cols label) // label where
    arrow = char symbol // upwardsArrow (rows content - 1)
    body = arrow <+> content

downwardsTab :: Char -> Box -> Box -> Box
downwardsTab symbol label content =
  label // downwardsSeparator (cols label) // body where
    arrow = downwardsArrow (rows content - 1) // char symbol
    body = arrow <+> content
