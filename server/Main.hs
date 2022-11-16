import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " PORT PATH"

app :: FilePath -> Application
app =
  staticApp . defaultFileServerSettings

main :: IO ()
main = do
  (port, path) <- getArgs >>= \case
    [port, path] -> return (port, path)
    _ -> getProgName >>= die . usage

  run (read port) (app path)
