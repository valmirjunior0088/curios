import qualified Curios.Parsing as Parsing (expression)
import Curios.Expression (Expression)
import Curios.Visualization.Expression (exToBox)
import Curios.Visualization.Term (teToBox)
import Curios.Translation (exToTerm)
import qualified Text.Megaparsec as Megaparsec (parse, eof)
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
          term <- exToTerm expression
          putStr "BEGIN\n"
          putStr "---\n"
          putStr input
          putStr "---\n"
          putStr (printf "%s\n" (show expression))
          putStr "---\n"
          putStr (render (exToBox expression))
          putStr "---\n"
          putStr (render (teToBox term))
          putStr "---\n"
          putStr "END\n"