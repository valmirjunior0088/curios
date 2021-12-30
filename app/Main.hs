import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Curios (run)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT"

main :: IO ()
main = do
  arguments <- getArgs

  input <- case arguments of
    input : [] -> return input
    _ -> getProgName >>= die . usage

  run input
