module WebAssembly.Encode.Ieee754
  ( ieee754Single
  , ieee754Double
  )
  where

-- This module assumes that GHC uses IEEE-754
-- floats (which it does) and reuses them

-- For completeness sake in the future this
-- module should house an actual IEEE-754 encoder

import Data.Word (Word32, Word64)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

ieee754Single :: Float -> Word32
ieee754Single = castFloatToWord32

ieee754Double :: Double -> Word64
ieee754Double = castDoubleToWord64
