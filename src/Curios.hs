module Curios
  ( run
  )
  where

import qualified Curios.Source.Parser as Source
import qualified Curios.Source.Error as Source
import qualified Curios.Core.Elaboration as Core
import qualified Curios.Core.Error as Core
import System.Exit (die)

run :: String -> IO ()
run input = do
  source <- readFile input

  program <- case Source.parse input source of
    Left sourceError -> die (Source.showError source sourceError)
    Right program -> return program
  
  _ <- case Core.elaborate program of
    Left coreError -> die (Core.showError source coreError)
    Right package -> return package
  
  putStrLn "lgtm"
