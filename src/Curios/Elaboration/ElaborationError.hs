module Curios.Elaboration.ElaborationError
  (ElaborationError (..)
  ,showElaborationError
  )
  where

import Curios.Core (Name)
import Curios.Core.TypeError (TypeError, showTypeError)
import Curios.PrettyPrinting.Framed (framed)
import Text.Megaparsec (SourcePos)

data ElaborationError =
  EeUndeclaredName SourcePos Name |
  EeRepeatedlyDeclaredName SourcePos Name |
  EeRepeatedlyDefinedName SourcePos Name |
  EeTypeError TypeError Name

showElaborationError :: ElaborationError -> String -> String
showElaborationError elaborationError source =
  case elaborationError of
    EeUndeclaredName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is undeclared" ++ "\n"
    EeRepeatedlyDeclaredName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is repeatedly declared" ++ "\n"
    EeRepeatedlyDefinedName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is repeatedly defined" ++ "\n"
    EeTypeError typeError name ->
      showTypeError typeError name source
