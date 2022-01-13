module Curios.Compilation.Ieee754
  ( ieee754Single
  , ieee754Double
  )
  where

import Data.Word (Word32, Word64)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

ieee754Single :: Float -> Word32
ieee754Single = castFloatToWord32

ieee754Double :: Double -> Word64
ieee754Double = castDoubleToWord64
