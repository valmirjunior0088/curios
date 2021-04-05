module Curios.Error
  (Error (..)
  ,showError
  )
  where

import Curios.Source.ParsingError (ParsingError, showParsingError)
import Curios.Elaboration.ElaborationError (ElaborationError, showElaborationError)

data Error =
  ErParsing ParsingError |
  ErElaboration ElaborationError

showError :: Error -> String -> String
showError curiosError source =
  case curiosError of
    ErParsing parsingError -> showParsingError parsingError source
    ErElaboration elaborationError -> showElaborationError elaborationError source
