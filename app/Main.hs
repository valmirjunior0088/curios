import qualified Curios.Parsing as Parsing (expression)
import qualified Text.Megaparsec as Megaparsec (parse, eof)

import Curios.Translation (exToTerm)
import Curios.Visualization.Expression (exToBox)
import Curios.Visualization.Term (teToBox)
import Curios.Context (coEmpty)
import Curios.Environment (enEmpty)
import Curios.Typechecking (trCheck)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.PrettyPrint.Boxes (render)
import Text.Printf (printf)

main :: IO ()
main =
  do
    input <- getContents
    case Megaparsec.parse (Parsing.expression <* Megaparsec.eof) "" input of
      Left errorBundle ->
        putStr (errorBundlePretty errorBundle)
      Right expression ->
        do
          let term = exToTerm expression
          putStr "SOURCE TEXT\n"
          putStr "----------\n"
          putStr input
          putStr "----------\n"
          putStr "\n"
          putStr "\n"
          putStr "\n"
          putStr "EXPRESSION REPRESENTATION\n"
          putStr "----------\n"
          putStr (printf "%s\n" (show expression))
          putStr "----------\n"
          putStr (render (exToBox expression))
          putStr "----------\n"
          putStr "\n"
          putStr "\n"
          putStr "\n"
          putStr "TERM REPRESENTATION\n"
          putStr "----------\n"
          putStr (printf "%s\n" (show term))
          putStr "----------\n"
          putStr (render (teToBox term))
          putStr "----------\n"
          putStr "\n"
          putStr "\n"
          putStr "\n"
          putStr "TYPE\n"
          putStr "----------\n"
          putStr (printf "%s\n" (show (trCheck coEmpty enEmpty term)))
          putStr "----------\n"
