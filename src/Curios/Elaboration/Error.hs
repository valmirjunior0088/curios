module Curios.Elaboration.Error
  (Kind (..)
  ,Error (..)
  ,throw
  ,showError
  )
  where

import Curios.Core (Name)
import Curios.PrettyPrinting.Megaparsec (showFile, showSource)
import Text.Megaparsec (SourcePos (..))

import qualified Curios.Core.Error as Core

data Kind =
  KnUndeclaredName |
  KnRepeatedlyDeclaredName |
  KnRepeatedlyDefinedName |
  KnCoreError Core.Error

data Error =
  Error { erSourcePos :: SourcePos, erName :: Name, erKind :: Kind }

throw :: SourcePos -> Name -> Kind -> Either Error a
throw sourcePos name kind =
  Left (Error { erSourcePos = sourcePos, erName = name, erKind = kind })

showMessage :: SourcePos -> String -> String -> String
showMessage sourcePos source message =
  showFile sourcePos
    ++ "\n"
    ++ showSource sourcePos source
    ++ "\n"
    ++ message ++ "\n"

showError :: String -> Error -> String
showError source (Error { erSourcePos, erName, erKind }) =
  case erKind of
    KnUndeclaredName ->
      showMessage erSourcePos source ("The name \"" ++ erName ++ "\" is undeclared")
    KnRepeatedlyDeclaredName ->
      showMessage erSourcePos source ("The name \"" ++ erName ++ "\" is repeatedly declared")
    KnRepeatedlyDefinedName ->
      showMessage erSourcePos source ("The name \"" ++ erName ++ "\" is repeatedly defined")
    KnCoreError coreError ->
      showFile erSourcePos
        ++ "When elaborating \"" ++ erName ++ "\"..." ++ "\n"
        ++ "\n"
        ++ Core.showError source coreError
