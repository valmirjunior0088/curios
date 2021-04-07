module Curios.Elaboration.Error
  (Error (..)
  ,showError
  )
  where

import Prelude hiding (error)

import Curios.Core (Name)
import Curios.PrettyPrinting.Framed (framed)
import Text.Megaparsec (SourcePos)
import qualified Curios.Core.Error as Core

data Error =
  ErUndeclaredName SourcePos Name |
  ErRepeatedlyDeclaredName SourcePos Name |
  ErRepeatedlyDefinedName SourcePos Name |
  ErCoreError Core.Error Name

showError :: Error -> String -> String
showError error source =
  case error of
    ErUndeclaredName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is undeclared" ++ "\n"
    ErRepeatedlyDeclaredName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is repeatedly declared" ++ "\n"
    ErRepeatedlyDefinedName sourcePos name ->
      framed sourcePos source
        ++ "\n"
        ++ "The name \"" ++ name ++ "\" is repeatedly defined" ++ "\n"
    ErCoreError coreError name ->
      Core.showError name coreError source
