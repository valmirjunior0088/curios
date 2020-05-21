module Curios.Visualization.Identifier
  (idToBox
  )
  where

import Curios.Identifier
  (Identifier (..)
  )

import Text.PrettyPrint.Boxes
  (Box
  ,text
  )

idToBox :: Identifier -> Box
idToBox identifier =
  text (show identifier)