module Curios
  (parse
  ,check
  )
  where

import Prelude hiding (error)

import Curios.Error (Error (..), erParsing)
import Curios.Core.Context (Context (..))
import Curios.Source.Types (Program)
import Curios.Core.Term (Origin (..))
import Curios.Translation (pgCheck)
import Text.Megaparsec (ParseErrorBundle (..), errorOffset)
import Text.Megaparsec.Stream (reachOffset)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Bifunctor (first)
import qualified Curios.Source.Parser as Parser
import qualified Text.Megaparsec as Megaparsec

parse :: String -> String -> Either Error Program
parse file source =
  first transform (Megaparsec.parse Parser.program file source) where
    transform parseErrorBundle =
      erParsing (OrSource sourcePos) error where
        (error :| _) = bundleErrors parseErrorBundle
        (sourcePos, _, _) = reachOffset (errorOffset error) (bundlePosState parseErrorBundle)

check :: String -> String -> Either Error Context
check file source =
  parse file source >>= pgCheck
