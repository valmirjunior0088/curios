module Curios.Source.ParsingError
  (ParsingError (..)
  ,fromParseErrorBundle
  ,showParsingError
  )
  where

import Curios.PrettyPrinting.Framed (framed)
import Data.List (intercalate)
import Data.Maybe (maybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Void (Void, absurd)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty (..))
import Text.Megaparsec.Pos (unPos)
import Text.Megaparsec.Error (ParseErrorBundle (..), errorOffset)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty

import Text.Megaparsec
  (Token
  ,SourcePos (..)
  ,ParseError (..)
  ,ErrorItem (..)
  ,ErrorFancy (..)
  ,Stream (..)
  )

data ParsingError =
  ParsingError SourcePos (ParseError String Void)

fromParseErrorBundle :: (ParseErrorBundle String Void) -> ParsingError
fromParseErrorBundle parseErrorBundle =
  ParsingError sourcePos parseError where
    (parseError :| _) = bundleErrors parseErrorBundle
    (sourcePos, _, _) = reachOffset (errorOffset parseError) (bundlePosState parseErrorBundle)

orList :: NonEmpty String -> String
orList tokens =
  case tokens of
    (item :| []) -> item
    (item :| [item']) -> item ++ " or " ++ item'
    _ -> intercalate ", " (NonEmpty.init tokens) ++ ", or " ++ NonEmpty.last tokens

showParseTokens :: String -> Set String -> String
showParseTokens prefix tokens =
  if Set.null tokens
    then ""
    else prefix ++ (orList . NonEmpty.fromList . Set.toAscList) tokens

showErrorItem :: ErrorItem (Token String) -> String
showErrorItem errorItem =
  case errorItem of
    Tokens token -> showTokens (Proxy :: Proxy String) token
    Label label -> NonEmpty.toList label
    EndOfInput -> "<end of input>"

showErrorFancy :: ErrorFancy Void -> String
showErrorFancy errorsFancy =
  case errorsFancy of
    ErrorFail message ->
      "Parsing error: explicit failure" ++ "\n"
        ++ message
    ErrorIndentation ordering reference actual ->
      let
        sign =
          case ordering of
            LT -> "> "
            EQ -> "= "
            GT -> "< "
      in
        "Parsing error: incorrect indentation" ++ "\n"
          ++ "- Expected: " ++ sign ++ show (unPos reference) ++ "\n"
          ++ "- Obtained: " ++ "  " ++ show (unPos actual) ++ "\n"
    ErrorCustom void ->
      absurd void

showParseError :: ParseError String Void -> String
showParseError parseError =
  case parseError of
    TrivialError _ obtained expected ->
      if Set.null expected && isNothing obtained
        then "Parsing error: unknown trivial error" ++ "\n"
        else 
          "Parsing error: unexpected token" ++ "\n"
            ++ showParseTokens "- Expected: " (showErrorItem `Set.map` expected) ++ "\n"
            ++ showParseTokens "- Obtained: " (showErrorItem `Set.map` maybe Set.empty Set.singleton obtained) ++ "\n"
    FancyError _ errors ->
      if Set.null errors
        then "Parsing error: unknown fancy error" ++ "\n"
        else unlines (showErrorFancy <$> Set.toAscList errors) ++ "\n"

showParsingError :: ParsingError -> String -> String
showParsingError (ParsingError sourcePos parseError) source =
  framed sourcePos source
    ++ "\n"
    ++ showParseError parseError ++ "\n"
