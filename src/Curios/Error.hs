module Curios.Error
  (Kind (..)
  ,Error (..)
  ,erParsing
  ,erUndeclaredName
  ,erRepeatedlyDeclaredName
  ,erRepeatedlyDefinedName
  ,erMismatchedFunctionType
  ,erMismatchedType
  ,erNonInferable
  ,orList
  ,showParseTokens
  ,showErrorItem
  ,showErrorFancy
  ,showParseError
  ,showErrorOrigin
  ,showErrorKind
  ,showError
  )
  where

import Prelude hiding (error)

import Curios.Formatting (showFramed)
import Curios.Core.Term (Origin (..), Name, Type, showTerm)
import Text.Megaparsec.Pos (unPos)
import Data.List (intercalate)
import Data.Maybe (maybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Void (Void, absurd)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty (..))
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

data Kind =
  KnParsing (ParseError String Void) |
  KnUndeclaredName Name |
  KnRepeatedlyDeclaredName Name |
  KnRepeatedlyDefinedName Name |
  KnMismatchedFunctionType Type |
  KnMismatchedType Type Type |
  KnNonInferable

data Error =
  Error { erOrigin :: Origin, erKind :: Kind }

erParsing :: Origin -> (ParseError String Void) -> Error
erParsing origin parseError =
  Error { erOrigin = origin, erKind = KnParsing parseError }

erUndeclaredName :: Origin -> Name -> Error
erUndeclaredName origin name =
  Error { erOrigin = origin, erKind = KnUndeclaredName name }

erRepeatedlyDeclaredName :: Origin -> Name -> Error
erRepeatedlyDeclaredName origin name =
  Error { erOrigin = origin, erKind = KnRepeatedlyDeclaredName name }

erRepeatedlyDefinedName :: Origin -> Name -> Error
erRepeatedlyDefinedName origin name =
  Error { erOrigin = origin, erKind = KnRepeatedlyDefinedName name }

erMismatchedFunctionType :: Origin -> Type -> Error
erMismatchedFunctionType origin obtained =
  Error { erOrigin = origin, erKind = KnMismatchedFunctionType obtained }

erMismatchedType :: Origin -> Type -> Type -> Error
erMismatchedType origin expected obtained =
  Error { erOrigin = origin, erKind = KnMismatchedType expected obtained }

erNonInferable :: Origin -> Error
erNonInferable origin =
  Error { erOrigin = origin, erKind = KnNonInferable }

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
      "Parsing error: explicit failure." ++ "\n" ++
        message
    ErrorIndentation ordering reference actual ->
      let
        sign =
          case ordering of
            LT -> "> "
            EQ -> "= "
            GT -> "< "
      in
        "Parsing error: incorrect indentation." ++ "\n" ++
          "- Expected: " ++ sign ++ show (unPos reference) ++ "\n" ++
          "- Obtained: " ++ "  " ++ show (unPos actual) ++ "\n"
    ErrorCustom void ->
      absurd void

showParseError :: ParseError String Void -> String
showParseError parseError =
  case parseError of
    TrivialError _ obtained expected ->
      if Set.null expected && isNothing obtained
        then "Parsing error: unknown trivial error." ++ "\n"
        else 
          "Parsing error: unexpected token." ++ "\n" ++
            showParseTokens "- Expected: " (showErrorItem `Set.map` expected) ++ "\n" ++
            showParseTokens "- Obtained: " (showErrorItem `Set.map` maybe Set.empty Set.singleton obtained) ++ "\n"
    FancyError _ errors ->
      if Set.null errors
        then "Parsing error: unknown fancy error." ++ "\n"
        else unlines (showErrorFancy <$> Set.toAscList errors) ++ "\n"

showErrorOrigin :: String -> String -> Origin -> String
showErrorOrigin file source origin =
  "In file " ++ file ++ "..." ++ "\n" ++
    case origin of
      OrMachine ->
        "In a machine-generated term..." ++ "\n"
      OrSource sourcePos ->
        let
          line = unPos (sourceLine sourcePos)
          column = unPos (sourceColumn sourcePos)
        in
          "In line " ++ show line ++ ", column " ++ show column ++ "..." ++ "\n" ++
            "\n" ++
            showFramed 3 60 (line - 1) (column - 1) source ++
            "\n"

showErrorKind :: Kind -> String
showErrorKind kind =
  case kind of
    KnParsing parseError ->
      showParseError parseError ++ "\n"
    KnUndeclaredName name ->
      "The name \"" ++ name ++ "\" is undeclared." ++ "\n"
    KnRepeatedlyDeclaredName name ->
      "The name \"" ++ name ++ "\" is repeatedly declared." ++ "\n"
    KnRepeatedlyDefinedName name ->
      "The name \"" ++ name ++ "\" is repeatedly defined." ++ "\n"
    KnMismatchedFunctionType obtained ->
      "Type mismatch." ++ "\n" ++
        "- Expected: <function type>" ++ "\n" ++
        "- Obtained: " ++ showTerm obtained ++ "\n"
    KnMismatchedType expected obtained ->
      "Type mismatch." ++ "\n" ++
        "- Expected: " ++ showTerm expected ++ "\n" ++
        "- Obtained: " ++ showTerm obtained ++ "\n"
    KnNonInferable ->
      "The term does not have an inferable type without an annotation." ++ "\n"

showError :: String -> String -> Error -> String
showError file source error =
  "Check failed." ++ "\n" ++
    "\n" ++
    showErrorOrigin file source (erOrigin error) ++
    showErrorKind (erKind error)
