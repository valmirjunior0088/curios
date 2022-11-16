module WebAssembly.Buffer
  ( Buffer (..)
  , byte
  , unsigned
  , unsignedFixed
  , signed
  , signedFixed
  , single
  , double
  , prependSize
  , bytesFrom
  , RelocBuffer (..)
  , relocEmpty
  , relocSingleton
  , relocPrependSize
  )
  where

import WebAssembly.Encode.Leb128 (Uleb128, uleb128, uleb128Fixed, Sleb128, sleb128, sleb128Fixed)
import WebAssembly.Encode.Ieee754 (ieee754Single, ieee754Double)
import WebAssembly.Syntax.Conventions (SymIdx)
import WebAssembly.Syntax.LLVM (RelocEntry (RelocEntry), RelocType)
import Data.ByteString.Lazy (unpack)
import Data.ByteString.Builder (Builder, word8, word32LE, word64LE, toLazyByteString)
import Data.Word (Word8, Word32)

data Buffer = Buffer
  { size :: Word32
  , contents :: Builder
  }

instance Semigroup Buffer where
  (<>) one other =
    Buffer (size + size') (contents <> contents') where
      Buffer { size = size, contents = contents } = one
      Buffer { size = size', contents = contents' } = other

instance Monoid Buffer where
  mempty = Buffer { size = 0, contents = mempty }

byte :: Word8 -> Buffer
byte = Buffer 1 . word8

unsigned :: Uleb128 a => a -> Buffer
unsigned = mconcat . map byte . uleb128

unsignedFixed :: Uleb128 a => Int -> a -> Buffer
unsignedFixed width = mconcat . map byte . uleb128Fixed width

signed :: Sleb128 a => a -> Buffer
signed = mconcat . map byte . sleb128

signedFixed :: Sleb128 a => Int -> a -> Buffer
signedFixed width = mconcat . map byte . sleb128Fixed width

single :: Float -> Buffer
single = Buffer 4 . word32LE . ieee754Single

double :: Double -> Buffer
double = Buffer 8 . word64LE . ieee754Double

prependSize :: Buffer -> Buffer
prependSize buffer@Buffer { size } = unsigned size <> buffer

bytesFrom :: Buffer -> [Word8]
bytesFrom Buffer { contents } = unpack (toLazyByteString contents)

data RelocBuffer = RelocBuffer
  { target :: Buffer
  , relocs :: [RelocEntry]
  }

addOffset :: Word32 -> RelocEntry -> RelocEntry
addOffset offset (RelocEntry relocType offset' symIdx) =
  RelocEntry relocType (offset + offset') symIdx

adjustOffsets :: Buffer -> [RelocEntry] -> [RelocEntry]
adjustOffsets Buffer { size } = map (addOffset size)

instance Semigroup RelocBuffer where
  (<>) one other =
    RelocBuffer (target <> target') (relocs ++ adjustOffsets target relocs') where
      RelocBuffer { target = target, relocs = relocs } = one
      RelocBuffer { target = target', relocs = relocs' } = other

instance Monoid RelocBuffer where
  mempty = RelocBuffer { target = mempty, relocs = [] }

relocEmpty :: Buffer -> RelocBuffer
relocEmpty target = RelocBuffer { target, relocs = [] }

relocSingleton :: Buffer -> RelocType -> SymIdx -> RelocBuffer
relocSingleton target relocType symIdx =
  RelocBuffer { target, relocs = [RelocEntry relocType 0 symIdx] }

relocPrependSize :: RelocBuffer -> RelocBuffer
relocPrependSize buffer@RelocBuffer { target = Buffer { size } } =
  relocEmpty (unsigned size) <> buffer
