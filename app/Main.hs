import qualified Curios.Syntax.Parser as Parser
import qualified Text.Megaparsec as Megaparsec

import Text.Megaparsec.Error (errorBundlePretty)
import CommandOptions (CommandOptions (..), coMain)
import Curios.Syntax.Expression (Name (..))
import Curios.Core.Context (cnLookupBinding, cnLookupDefinition)
import Curios.Translation (exTranslate, pgTranslate)

run :: CommandOptions -> IO ()
run commandOptions =
  case commandOptions of
    CoPrint string ->
      case Megaparsec.parse (Parser.expression <* Megaparsec.eof) "" string of
        Left errorBundle ->
          putStr (errorBundlePretty errorBundle)

        Right expression ->
          do
            putStrLn ("Expression")
            putStrLn (show expression)
            putStrLn ("")
            putStrLn ("Term")
            putStrLn (show (exTranslate expression))

    CoCheck path maybeName ->
      do
        file <- readFile path

        case Megaparsec.parse (Parser.program <* Megaparsec.eof) "" file of
          Left errorBundle ->
            putStr (errorBundlePretty errorBundle)

          Right program ->
            case pgTranslate program of
              Left translationError ->
                putStrLn ("Check failed [" ++ show translationError ++ "]")

              Right context ->
                case maybeName of
                  Nothing -> 
                    putStrLn ("Check succeeded!")
                  
                  Just name ->
                    do
                      putStrLn ("Binding [" ++ show name ++ "]")
                      putStrLn (show (cnLookupBinding (Name name) context))
                      putStrLn ("")
                      putStrLn ("Definition [" ++ show name ++ "]")
                      putStrLn (show (cnLookupDefinition (Name name) context))

main :: IO ()
main =
  coMain run