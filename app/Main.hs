import qualified Curios.Parsing as Parsing (expression, statements)
import qualified Text.Megaparsec as Megaparsec (parse, eof)

import CommandOptions (CommandOptions (..), coMain)
import Curios.Expression (Name (..))
import Curios.Translation (exToTerm, cnInsertStatements)
import Curios.Visualization.Expression (exToBox)
import Curios.Visualization.Term (teToBox)
import Curios.Context (cnEmpty, cnLookup)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.PrettyPrint.Boxes (render)
import Text.Printf (printf)

run :: CommandOptions -> IO ()
run commandOptions =
  case commandOptions of
    CoPrint sourceExpression ->
      case Megaparsec.parse (Parsing.expression <* Megaparsec.eof) "" sourceExpression of
        Left errorBundle ->
          putStr (errorBundlePretty errorBundle)

        Right expression ->
          do
            let term = exToTerm expression
            putStr "SOURCE EXPRESSION\n"
            putStr "----------\n"
            putStr sourceExpression
            putStr "----------\n"
            putStr "\n"
            putStr "\n"
            putStr "\n"
            putStr "EXPRESSION\n"
            putStr "----------\n"
            putStr (printf "%s\n" (show expression))
            putStr "----------\n"
            putStr (render (exToBox expression))
            putStr "----------\n"
            putStr "\n"
            putStr "\n"
            putStr "\n"
            putStr "TERM\n"
            putStr "----------\n"
            putStr (printf "%s\n" (show term))
            putStr "----------\n"
            putStr (render (teToBox term))
            putStr "----------\n"

    CoCheck path maybeName ->
      do
        file <- readFile path

        case Megaparsec.parse (Parsing.statements <* Megaparsec.eof) "" file of
          Left errorBundle ->
            putStr (errorBundlePretty errorBundle)

          Right statements ->
            case cnInsertStatements statements cnEmpty of
              Left message ->
                putStr ("Typechecking failed [" ++ message ++ "]\n")

              Right context ->
                case maybeName of
                  Nothing -> 
                    putStr "Typechecking succeeded!"
                  
                  Just name ->
                    case cnLookup (Name name) context of
                      Left message ->
                        putStr ("Name lookup failed [" ++ message ++ "]\n")

                      Right term ->
                        do
                          putStr "TERM\n"
                          putStr "----------\n"
                          putStr (printf "%s\n" (show term))
                          putStr "----------\n"
                          putStr (render (teToBox term))
                          putStr "----------\n"

main :: IO ()
main =
  coMain run