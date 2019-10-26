import Curios.Parsing
  ( statement
  )

import Text.Megaparsec
  ( parseTest
  )

main :: IO ()
main =
  parseTest statement example

example :: String
example =
  unlines
    [ "( package examples"
    , "  ( define id <a:type. a. a>"
    , "    {a. value. value}"
    , "  )"
    , ""
    , "( package pair"
    , "  ( define pair <type. type. type>"
    , "    {a. b. <c:type. <a. b. c.>. c>}"
    , "  )"
    , ""
    , "  ( define make <a:type. b:type. a. b. pair a b>"
    , "    {a. b. x. y. {c. f. f x y}}"
    , "  )"
    , ")"
    )

{-
The generated AST will be here
once the test works.
-}
