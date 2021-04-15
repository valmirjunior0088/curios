module Curios.PrettyPrinting.Framed
  (framed
  )
  where
  
import Text.PrettyPrint.Boxes
  (Box
  ,char
  ,text
  ,(<+>)
  ,(//)
  ,hcat
  ,vcat
  ,top
  ,left
  ,right
  ,alignHoriz
  ,cols
  ,render
  )

label :: Int -> Box -> Box
label width box =
  alignHoriz right width box

regularRow :: Int -> (Box, [(Bool, Box)]) -> Box
regularRow labelWidth (labelBox, columnBoxes) =
  label labelWidth labelBox <+> char '│' <+> hcat top (map snd columnBoxes)

highlightedRow :: Int -> (Box, [(Bool, Box)]) -> Box
highlightedRow labelWidth (labelBox, columnBoxes) =
  label labelWidth labelBox <+> (char '├' // char '·') <+> hcat top (map transform columnBoxes) where
    transform (isHighlighted, columnBox) =
      columnBox // char (if isHighlighted then '┴' else '─')

assemble :: [(Box, [(Bool, Box)])] -> Box
assemble rows =
  vcat left (map transform rows) where
    labelWidth =
      foldl max 0 (map (cols . fst) rows)
    transform row =
      if any fst (snd row)
        then highlightedRow labelWidth row
        else regularRow labelWidth row

enumerate :: [a] -> [(Int, a)]
enumerate =
  zip [0..]

within :: (Int, Int) -> (Int, a) -> Bool
within (firstIndex, lastIndex) (index, _) =
  elem index [firstIndex .. lastIndex]

source :: (Int, Int) -> (Int, Int) -> String -> [(Int, [(Int, Char)])]
source lineRange columnRange =
  map transform . filter (within lineRange) . enumerate . lines where
    transform (lineNumber, lineColumns) =
      (lineNumber, filter (within columnRange) (enumerate lineColumns))

boxed :: Int -> Int -> Int -> Int -> String -> [(Box, [(Bool, Box)])]
boxed lineQuantity columnQuantity highlightedLine highlightedColumn =
  map transformLines . source lineRange columnRange where
    lineRange =
      (highlightedLine - lineQuantity, highlightedLine + lineQuantity)
    columnRange =
      (highlightedColumn - columnQuantity, highlightedColumn + columnQuantity)
    transformColumns lineNumber (columnNumber, columnContent) =
      (highlightedLine == lineNumber && highlightedColumn == columnNumber, char columnContent)
    transformLines (lineNumber, lineColumns) =
      (text (show (lineNumber + 1)), map (transformColumns lineNumber) lineColumns)

framed :: Int -> Int -> Int -> Int -> String -> String
framed lineQuantity columnQuantity highlightedLine highlightedColumn =
  render . assemble . boxed lineQuantity columnQuantity highlightedLine highlightedColumn
