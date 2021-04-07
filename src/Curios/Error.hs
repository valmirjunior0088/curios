module Curios.Error
  (Error (..)
  ,showError
  )
  where

import Prelude hiding (error)

import qualified Curios.Source.Error as Source
import qualified Curios.Elaboration.Error as Elaboration

data Error =
  ErSource Source.Error |
  ErElaboration Elaboration.Error

showError :: Error -> String -> String
showError error source =
  case error of
    ErSource sourceError -> Source.showError sourceError source
    ErElaboration elaborationError -> Elaboration.showError elaborationError source
