import Curios.Expression
import Curios.Program
import Curios.Parsing

import Text.Megaparsec
import Text.Megaparsec.Debug

main :: IO ()
main = parseTest (dbg "statements" (some statement)) exampleStatements

exampleStatements =
  "assume either : <type! type! type>\n\
 \ assume left : <a:type. b:type! (either a b)>\n\
 \ assume right : <a:type! b:type. (either a b)>"
