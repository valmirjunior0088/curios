module Curios.Error
  (Error (..)
  ,erParsing
  ,erEeUndeclaredName
  ,erEeRepeatedlyDeclaredName
  ,erEeRepeatedlyDefinedName
  ,erTeUndeclaredName
  ,erTeMismatchedFunctionType
  ,erTeMismatchedType
  ,erTeNonInferable
  ,showError
  )
  where

import Curios.Core (Origin, Name, Type)
import Data.Void (Void)
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Error (ParseErrorBundle)

import Curios.Source.ParsingError
  (ParsingError (..)
  ,fromParseErrorBundle
  ,showParsingError
  )

import Curios.Elaboration.ElaborationError
  (ElaborationError (..)
  ,eeUndeclaredName
  ,eeRepeatedlyDeclaredName
  ,eeRepeatedlyDefinedName
  ,showElaborationError
  )

import Curios.Core.TypeError
  (TypeError (..)
  ,teUndeclaredName
  ,teMismatchedFunctionType
  ,teMismatchedType
  ,teNonInferable
  ,showTypeError
  )

data Error =
  ErParsing ParsingError |
  ErElaboration ElaborationError |
  ErType TypeError

erParsing :: (ParseErrorBundle String Void) -> Error
erParsing parseErrorBundle =
  ErParsing (fromParseErrorBundle parseErrorBundle)

erEeUndeclaredName :: SourcePos -> Name -> Error
erEeUndeclaredName sourcePos name =
  ErElaboration (eeUndeclaredName sourcePos name)

erEeRepeatedlyDeclaredName :: SourcePos -> Name -> Error
erEeRepeatedlyDeclaredName sourcePos name =
  ErElaboration (eeRepeatedlyDeclaredName sourcePos name)

erEeRepeatedlyDefinedName :: SourcePos -> Name -> Error
erEeRepeatedlyDefinedName sourcePos name =
  ErElaboration (eeRepeatedlyDefinedName sourcePos name)

erTeUndeclaredName :: Origin -> Name -> Error
erTeUndeclaredName origin name =
  ErType (teUndeclaredName origin name)

erTeMismatchedFunctionType :: Origin -> Type -> Error
erTeMismatchedFunctionType origin obtained =
  ErType (teMismatchedFunctionType origin obtained)

erTeMismatchedType :: Origin -> Type -> Type -> Error
erTeMismatchedType origin expected obtained =
  ErType (teMismatchedType origin expected obtained)

erTeNonInferable :: Origin -> Error
erTeNonInferable origin =
  ErType (teNonInferable origin)

showError :: Error -> String -> String
showError curiosError source =
  case curiosError of
    ErParsing parsingError -> showParsingError parsingError source
    ErElaboration elaborationError -> showElaborationError elaborationError source
    ErType typeError -> showTypeError typeError source
