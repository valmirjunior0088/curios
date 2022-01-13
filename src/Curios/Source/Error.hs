{-# LANGUAGE NamedFieldPuns #-}

module Curios.Source.Error
  ( Error (..)
  , fromErrorBundle
  , showError
  )
  where

import Curios.PrettyPrinting.Megaparsec (showFile, showSource)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Void (Void, absurd)
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

data Error =
  Error SourcePos (ParseError String Void)

fromErrorBundle :: ParseErrorBundle String Void -> Error
fromErrorBundle (ParseErrorBundle { bundleErrors, bundlePosState }) =
  Error pstateSourcePos parseError where
    parseError :| _ = bundleErrors
    (_, PosState { pstateSourcePos }) = reachOffset (errorOffset parseError) bundlePosState

orList :: NonEmpty String -> String
orList tokens =
  case tokens of
    item :| [] -> item
    item :| [item'] -> item ++ " or " ++ item'
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
          ++ "- Obtained: " ++ "  " ++ show (unPos actual)

    ErrorCustom void ->
      absurd void

showParseError :: ParseError String Void -> String
showParseError parseError =
  case parseError of
    TrivialError _ obtained expected ->
      if Set.null expected && isNothing obtained
        then "Parsing error: unknown trivial error"
        else 
          "Parsing error: unexpected token" ++ "\n"
            ++ showParseTokens "- Expected: " (showErrorItem `Set.map` expected) ++ "\n"
            ++ showParseTokens "- Obtained: " (showErrorItem `Set.map` maybe Set.empty Set.singleton obtained)
            
    FancyError _ errors ->
      if Set.null errors
        then "Parsing error: unknown fancy error"
        else unlines (showErrorFancy <$> Set.toAscList errors)

showError :: String -> Error -> String
showError source (Error sourcePos parseError) =
  showFile sourcePos
    ++ "\n"
    ++ showSource sourcePos source
    ++ "\n"
    ++ showParseError parseError
