import qualified Curios.Source.Parser as Source
import qualified Curios.Source.Error as Source
import qualified Curios.Core.Elaboration as Core
import qualified Curios.Core.Error as Core
import qualified Curios.Compilation.Erasure as Compilation
import qualified Curios.Compilation.Conversion as Compilation
import qualified Curios.Compilation.Flattening as Compilation
import qualified Curios.Compilation.Generation as Compilation
import qualified Curios.Compilation.Serialization as Compilation
import qualified Data.Map as Map
import System.Exit (die)
import System.Environment (getArgs, getProgName)
import Data.Function ((&))
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Builder (toLazyByteString)
import Prelude hiding (writeFile)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT OUTPUT"

main :: IO ()
main = do
  arguments <- getArgs

  (input, output) <- case arguments of
    input : output : [] -> return (input, output)
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case Source.parse input source of
    Left sourceError -> die (Source.showError source sourceError)
    Right program -> return program
  
  context <- case Core.elaborate program of
    Left coreError -> die (Core.showError source coreError)
    Right context -> return context

  let (_, definitions) = context in if Map.member "main" definitions
    then return ()
    else die "Program typechecked but no `main` definition was found"
  
  Compilation.erase context
    & fmap Compilation.convert
    & fmap Compilation.flatten
    & Compilation.generate
    & Compilation.serialize
    & toLazyByteString
    & writeFile output
