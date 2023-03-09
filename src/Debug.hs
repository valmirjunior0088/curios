module Debug
  ( framed
  )
  where

import Data.Ix (inRange)

type Range = (Int, Int)
type Grid = [(Int, [(Int, Char)])]

decompose :: String -> Grid
decompose = zip [1..] . map (zip [1..]) . lines

resize :: Range -> Range -> Grid -> Grid
resize rowRange columnRange grid = do
  let
    keepRow (index, _) = inRange rowRange index
    keepColumn (index, _) = inRange columnRange index

  [(index, filter keepColumn column) | (index, column) <- filter keepRow grid]

prepare :: Int -> Int -> Grid -> [(String, String)]
prepare x y grid = do
  let
    highlighter column = [if index == y then '┴' else '─' | (index, _) <- column]
    highlight (index, column) = (show index, map snd column) : [("", highlighter column) | index == x]

  concatMap highlight grid

padl :: Int -> String -> String
padl amount string = replicate (amount - length string) ' ' ++ string

assemble :: [(String, String)] -> String
assemble rows = do
  let
    padding = maximum (map (length . fst) rows)
    finish (number, line) = padl padding number ++ " │ " ++ line

  unlines (map finish rows)

framed :: Int -> Int -> Int -> Int -> String -> String
framed height width x y source = do
  let
    rowRange = (x - height, x + height)
    columnRange = (y - width, y + width)
    grid = decompose source
    resized = resize rowRange columnRange grid
    prepared = prepare x y resized

  assemble prepared
