{-# LANGUAGE QuasiQuotes #-}

import Curios.Expression
import Curios.Program
import qualified Curios.Parsing as Pa

import Text.Megaparsec
import Text.Megaparsec.Debug

main :: IO ()
main = parseTest Pa.statement example

example :: String
example =
  unlines [
    "module either where",
    "  assume either : <type! type! type>",
    "  assume left : <a:type! b:type. (either a b)>",
    "  assume right : <a:type. b:type! (either a b)>",
    "end"
  ]