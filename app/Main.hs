import Curios (check, evaluate)
import Curios.Error (showError)
import System.IO (IOMode (..), openFile, hGetContents)
import System.Environment (getArgs)

main :: IO ()
main = do
  arguments <- getArgs

  case arguments of
    [file, name] -> do
      source <- openFile file ReadMode >>= hGetContents

      case check file source of
        Left curiosError -> putStr (showError curiosError source)
        Right context -> putStr (evaluate name context)

    [file] -> do
      source <- openFile file ReadMode >>= hGetContents

      case check file source of
        Left curiosError -> putStr (showError curiosError source)
        Right _ -> putStr ("Check succeeded!" ++ "\n")

    _ -> do
      putStr ("USAGE: <command> file [name]" ++ "\n")
