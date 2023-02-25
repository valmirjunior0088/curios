import Error (showError)
import Core.Parse (parse)
import Core.Program (check)
import Inter.Translate (translate)
import Inter.Compile (compile)
import WebAssembly.Serialize (writeModule)
import System.Exit (die)
import System.Environment (getArgs, getProgName)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT OUTPUT"

main :: IO ()
main = do
  (input, output) <- getArgs >>= \case
    [input, output] -> return (input, output)
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case parse source of
    Left reason -> die (showError source reason)
    Right program -> return program

  bindings <- case check program of
    Left reason -> die (showError source reason)
    Right bindings -> return bindings

  writeModule output (compile $ translate bindings)
