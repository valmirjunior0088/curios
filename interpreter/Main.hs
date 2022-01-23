import qualified Curios.Source.Parser as Source
import qualified Curios.Source.Error as Source
import qualified Curios.Core.Term as Core
import qualified Curios.Core.Elaboration as Core
import qualified Curios.Core.Error as Core
import System.Exit (die)
import System.Environment (getArgs, getProgName)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT"

main :: IO ()
main = do
  arguments <- getArgs

  input <- case arguments of
    input : [] -> return input
    _ -> getProgName >>= die . usage
  
  source <- readFile input

  program <- case Source.parse input source of
    Left sourceError -> die (Source.showError source sourceError)
    Right program -> return program
  
  term <- case Core.evaluate program (Core.TrReference Nothing "main") of
    Left coreError -> die (Core.showError source coreError)
    Right term -> return term

  putStrLn (show term)
