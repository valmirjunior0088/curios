module WebAssembly.Encode.Utf8
  ( utf8Char
  , utf8String
  )
  where

import Data.Bits (Bits (..))
import Data.Word (Word8)

least6 :: Integral a => a -> Word8
least6 value = fromIntegral value .&. 0b00111111

utf8Char :: Char -> [Word8]
utf8Char character = case fromEnum character of
  value | value <= 0x7F -> 
    [ fromIntegral value
    ]
  
  value | value <= 0x7FF ->
    [ fromIntegral (shiftR value 6) .|. 0b11000000
    , least6 value .|. 0b10000000
    ]
  
  value | value <= 0xFFFF ->
    [ fromIntegral (shiftR value 12) .|. 0b11100000
    , least6 (shiftR value 6) .|. 0b10000000
    , least6 value .|. 0b10000000
    ]
  
  value | value <= 0x10FFFF ->
    [ fromIntegral (shiftR value 18) .|. 0b11110000
    , least6 (shiftR value 12) .|. 0b10000000
    , least6 (shiftR value 6) .|. 0b10000000
    , least6 value .|. 0b10000000
    ]
  
  _ -> error "character can't be represented in utf-8"

utf8String :: String -> [Word8]
utf8String = concatMap utf8Char
