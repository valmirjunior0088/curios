module Debug
  ( framed
  )
  where

type Grid = [(Int, [(Int, Char)])]

decompose :: String -> Grid
decompose = zip [1..] . map (zip [1..]) . lines

resize :: [Int] -> [Int] -> Grid -> Grid
resize rows columns grid = do
  let
    keepRow (index, _) = index `elem` rows
    keepColumn (index, _) = index `elem` columns

  [(index, filter keepColumn column) | (index, column) <- filter keepRow grid]

prepare :: Int -> Int -> Grid -> [(String, String)]
prepare x y grid = do
  let
    highlighter column = concat [if index == y then "┴" else "─" | (index, _) <- column]
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
    grid = decompose source
    rows = [x - height .. x + height]
    columns = [y - width .. y + width]
    resized = resize rows columns grid
    prepared = prepare x y resized

  assemble prepared
