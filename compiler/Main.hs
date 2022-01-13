import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Curios (run)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT OUTPUT"

main :: IO ()
main = do
  arguments <- getArgs

  (input, output) <- case arguments of
    input : output : [] -> return (input, output)
    _ -> getProgName >>= die . usage

  run input output
