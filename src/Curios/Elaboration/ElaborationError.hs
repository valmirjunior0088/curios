module Curios.Elaboration.ElaborationError
  (ElaborationError (..)
  ,eeUndeclaredName
  ,eeRepeatedlyDeclaredName
  ,eeRepeatedlyDefinedName
  ,showElaborationError
  )
  where

import Curios.Core.Term (Name)
import Curios.Formatting (framed)
import Text.Megaparsec (SourcePos)

data Kind =
  KnUndeclaredName Name |
  KnRepeatedlyDeclaredName Name |
  KnRepeatedlyDefinedName Name

data ElaborationError =
  ElaborationError { eeSourcePos :: SourcePos, eeKind :: Kind }

eeUndeclaredName :: SourcePos -> Name -> ElaborationError
eeUndeclaredName sourcePos name =
  ElaborationError { eeSourcePos = sourcePos, eeKind = KnUndeclaredName name }

eeRepeatedlyDeclaredName :: SourcePos -> Name -> ElaborationError
eeRepeatedlyDeclaredName sourcePos name =
  ElaborationError { eeSourcePos = sourcePos, eeKind = KnRepeatedlyDeclaredName name }

eeRepeatedlyDefinedName :: SourcePos -> Name -> ElaborationError
eeRepeatedlyDefinedName sourcePos name =
  ElaborationError { eeSourcePos = sourcePos, eeKind = KnRepeatedlyDefinedName name }

showKind :: Kind -> String
showKind kind =
  case kind of
    KnUndeclaredName name ->
      "The name \"" ++ name ++ "\" is undeclared" ++ "\n"
    KnRepeatedlyDeclaredName name ->
      "The name \"" ++ name ++ "\" is repeatedly declared" ++ "\n"
    KnRepeatedlyDefinedName name ->
      "The name \"" ++ name ++ "\" is repeatedly defined" ++ "\n"

showElaborationError :: ElaborationError -> String -> String
showElaborationError elaborationError source =
  framed (eeSourcePos elaborationError) source
    ++ "\n"
    ++ showKind (eeKind elaborationError)
