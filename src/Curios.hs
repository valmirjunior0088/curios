module Curios
  (parse
  ,check
  ,evaluate
  )
  where

import Prelude hiding (error)

import Curios.Core.Context (Context)
import Curios.Core.Term (Origin (..), Name, showTerm)
import Curios.Core.Context (Context (..), cnLookupDeclaration, cnLookupDefinition)
import Curios.Core.Verification (trReduce)
import Curios.Source.Types (Program)
import Curios.Translation (pgCheck)
import Curios.Error (Error, erParsing)
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

evaluate :: Name -> Context -> String
evaluate name context =
  "Check succeeded!" ++ "\n" ++
    "\n" ++
    case (cnLookupDeclaration name context, cnLookupDefinition name context) of
      (Just declaration, Just definition) ->
        "Declaration:" ++ "\n" ++
          showTerm declaration ++ "\n" ++
          "\n" ++
          "Definition:" ++ "\n" ++
          showTerm definition ++ "\n" ++
          "\n" ++
          "Evaluation:" ++ "\n" ++ 
          showTerm (trReduce (cnDefinitions context) definition) ++ "\n"
      _ ->
        "An undeclared name was supplied for evaluation." ++ "\n"
