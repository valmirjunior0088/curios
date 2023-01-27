module Error
  ( Origin (..)
  , Error (..)
  , showError
  , fromParseErrorBundle
  )
  where

import Debug (framed)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Text.Megaparsec.Pos (unPos)
import Text.Megaparsec.Error (ParseErrorBundle (..), errorOffset)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Text.Megaparsec
  ( Token
  , SourcePos (..)
  , ParseError (..)
  , ErrorItem (..)
  , ErrorFancy (..)
  , Stream (..)
  , PosState (..)
  , reachOffset
  , showTokens
  )

data Origin =
  Machine |
  Source SourcePos
  deriving (Show)

instance Eq Origin where
  (==) _ _ = True

data Error = Error
  { origin :: Origin
  , message :: String
  }

showSourcePos :: SourcePos -> String
showSourcePos SourcePos { sourceLine, sourceColumn } =
  "In line: " ++ show (unPos sourceLine) ++ ", column: " ++ show (unPos sourceColumn) ++ "..."

showFramed :: SourcePos -> String -> String
showFramed SourcePos { sourceLine, sourceColumn } =
  framed 3 30 (unPos sourceLine) (unPos sourceColumn)

showError :: String -> Error -> String
showError source Error { message, origin } = case origin of
  Machine -> "In a machine-generated term...\n\n" ++ message
  Source sourcePos -> showSourcePos sourcePos ++ "\n\n" ++ message ++ "\n\n" ++ showFramed sourcePos source

orList :: NonEmpty String -> String
orList = \case
  item :| [] -> item
  item :| [item'] -> item ++ " or " ++ item'
  tokens -> intercalate ", " (NonEmpty.init tokens) ++ ", or " ++ NonEmpty.last tokens

showParseTokens :: String -> Set String -> String
showParseTokens prefix tokens =
  if Set.null tokens then ""
    else prefix ++ orList (NonEmpty.fromList $ Set.toAscList tokens)

showErrorItem :: ErrorItem (Token String) -> String
showErrorItem = \case
  Tokens token -> showTokens (Proxy :: Proxy String) token
  Label label -> NonEmpty.toList label
  EndOfInput -> "<end of input>"

showErrorFancy :: ErrorFancy String -> String
showErrorFancy = \case
  ErrorFail message -> "Parsing error: explicit failure" ++ "\n" ++ message

  ErrorIndentation ordering reference actual -> do
    let sign = case ordering of LT -> "> "; EQ -> "= "; GT -> "< "

    "Parsing error: incorrect indentation" ++ "\n"
      ++ "- Expected: " ++ sign ++ show (unPos reference) ++ "\n"
      ++ "- Obtained: " ++ "  " ++ show (unPos actual)

  ErrorCustom message -> "Parsing error: " ++ message

showParseError :: ParseError String String -> String
showParseError = \case
  TrivialError _ obtained expected -> if Set.null expected && isNothing obtained
    then "Parsing error: unknown trivial error"

    else "Parsing error: unexpected token" ++ "\n"
      ++ showParseTokens "- Expected: " (showErrorItem `Set.map` expected) ++ "\n"
      ++ showParseTokens "- Obtained: " (showErrorItem `Set.map` maybe Set.empty Set.singleton obtained)

  FancyError _ errors -> if Set.null errors
    then "Parsing error: unknown fancy error"
    else unlines (showErrorFancy <$> Set.toAscList errors)

fromParseErrorBundle :: ParseErrorBundle String String -> Error
fromParseErrorBundle (ParseErrorBundle { bundleErrors, bundlePosState }) = do
  let
    parseError :| _ = bundleErrors
    (_, PosState { pstateSourcePos }) = reachOffset (errorOffset parseError) bundlePosState

  Error { message = showParseError parseError, origin = Source pstateSourcePos }
