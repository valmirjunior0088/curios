module Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,horizontalLine
  ,upwardsSeparator
  ,downwardsSeparator
  ,roofed
  ,floored
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

upwardsSeparator :: Int -> Box
upwardsSeparator size =
  if size <= 0 then nullBox else char '▲' <> horizontalLine (size - 1)

downwardsSeparator :: Int -> Box
downwardsSeparator size =
  if size <= 0 then nullBox else char '▼' <> horizontalLine (size - 1)

roofed :: Box -> Box
roofed box =
  upwardsSeparator (cols box) // box

floored :: Box -> Box
floored box =
  box // downwardsSeparator (cols box)

upwardsSeparated :: Foldable f => f Box -> Box
upwardsSeparated boxes =
  punctuateV left (upwardsSeparator size) boxes where
    size = foldl (\accumulator box -> max accumulator (cols box)) 0 boxes

downwardsSeparated :: Foldable f => f Box -> Box
downwardsSeparated boxes =
  punctuateV left (downwardsSeparator size) boxes where
    size = foldl (\accumulator box -> max accumulator (cols box)) 0 boxes

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
  body // roofed label where
    arrow = char symbol // upwardsArrow (rows content - 1)
    body = arrow <+> content

downwardsTab :: Char -> Box -> Box -> Box
downwardsTab symbol label content =
  floored label // body where
    arrow = downwardsArrow (rows content - 1) // char symbol
    body = arrow <+> content