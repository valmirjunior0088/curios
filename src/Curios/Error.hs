module Curios.Error
  (Kind (..)
  ,Error (..)
  ,erParsing
  ,erUndeclaredName
  ,erRepeatedlyDeclaredName
  ,erRepeatedlyDefinedName
  ,erMismatchedFunctionType
  ,erMismatchedType
  ,erFunctionNotInferable
  )
  where

import Curios.Core.Term (Origin (..), Name (..), Type)
import Text.Megaparsec.Error (ParseError (..))
import Data.Void (Void)

data Kind =
  KnParsing (ParseError String Void) |
  KnUndeclaredName Name |
  KnRepeatedlyDeclaredName Name |
  KnRepeatedlyDefinedName Name |
  KnMismatchedFunctionType Type |
  KnMismatchedType Type Type |
  KnFunctionNotInferable

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
erMismatchedFunctionType origin obtainedType =
  Error { erOrigin = origin, erKind = KnMismatchedFunctionType obtainedType }

erMismatchedType :: Origin -> Type -> Type -> Error
erMismatchedType origin expectedType obtainedType =
  Error { erOrigin = origin, erKind = KnMismatchedType expectedType obtainedType }

erFunctionNotInferable :: Origin -> Error
erFunctionNotInferable origin =
  Error { erOrigin = origin, erKind = KnFunctionNotInferable }
