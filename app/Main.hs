import Curios (run)
import System.Environment (getArgs)

main :: IO ()
main = do
  arguments <- getArgs

  case arguments of
    [file, name] -> readFile file >>= run file (Just name)
    [file] -> readFile file >>= run file Nothing
    _ -> putStr ("USAGE: <command> file [name]" ++ "\n")
