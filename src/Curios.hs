module Curios
  ( run
  )
  where

import qualified Curios.Source.Parser as Source
import qualified Curios.Source.Error as Source
import qualified Curios.Core.Elaboration as Core
import qualified Curios.Core.Error as Core
import qualified Curios.Compilation.Erasure as Compilation
import qualified Curios.Compilation.Conversion as Compilation
import qualified Curios.Compilation.Flattening as Compilation
import qualified Curios.Compilation.Generation as Compilation
import qualified Curios.Compilation.Serialization as Compilation
import System.Exit (die)
import Data.Function ((&))
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Builder (toLazyByteString)
import Prelude hiding (writeFile)

run :: String -> String -> IO ()
run input output = do
  source <- readFile input

  program <- case Source.parse input source of
    Left sourceError -> die (Source.showError source sourceError)
    Right program -> return program
  
  context <- case Core.elaborate program of
    Left coreError -> die (Core.showError source coreError)
    Right context -> return context
  
  items <- case Compilation.erase context of
    Nothing -> die "Program typechecked but no `main` definition was found"
    Just items -> return items
  
  fmap Compilation.convert items
    & fmap Compilation.flatten
    & Compilation.generate
    & Compilation.serialize
    & toLazyByteString
    & writeFile output
