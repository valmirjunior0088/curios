module Curios.Visualization.Common
  (parenthesized
  ,emphasized
  ,newlined
  ,horizontalLine
  ,roofed
  ,floored
  ,verticalLine
  ,upwardsArrow
  ,downwardsArrow
  )
  where

import Prelude hiding
  ((<>)
  )

import Text.PrettyPrint.Boxes
  (Box
  ,char
  ,hcat
  ,vcat
  ,left
  ,rows
  ,cols
  ,(<>)
  ,(<+>)
  ,(//)
  ,(/+/)
  )

parenthesized :: Box -> Box
parenthesized box =
  char '(' <> box <> char ')'

emphasized :: Box -> Box
emphasized box =
  char '▶' <+> box

newlined :: Box -> Box
newlined box =
  box // char ' '

horizontalLine :: Int -> Box
horizontalLine size =
  hcat left (replicate size (char '━'))

roofed :: Box -> Box
roofed box =
  (char '┏' <> horizontalLine (cols box - 1)) // box

floored :: Box -> Box
floored box =
  box // (char '┗' <> horizontalLine (cols box - 1))

verticalLine :: Int -> Box
verticalLine size =
  vcat left (replicate size (char '┃'))

upwardsArrow :: Int -> Box
upwardsArrow size =
  char '▲' // verticalLine (size - 1)

downwardsArrow :: Int -> Box
downwardsArrow size =
  verticalLine (size - 1) // char '▼'
