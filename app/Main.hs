import Prelude hiding (error)

import Curios (check)
import Curios.Error (showError)
import Curios.Context (showContext)
import System.IO (IOMode (..), openFile, hGetContents)
import System.Environment (getArgs)

main :: IO ()
main = do
  putStr ("Curios - a web of types" ++ "\n")

  arguments <- getArgs

  case arguments of
    [file, name] -> do
      source <- openFile file ReadMode >>= hGetContents

      case check file source of
        Left error -> putStr (showError file source error)
        Right context -> putStr (showContext name context)

    [file] -> do
      source <- openFile file ReadMode >>= hGetContents

      case check file source of
        Left error -> putStr (showError file source error)
        Right _ -> putStr ("Check succeeded!" ++ "\n")

    _ -> do
      putStr ("USAGE: <command> file [name]" ++ "\n")
