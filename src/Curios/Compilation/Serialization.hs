module Curios.Compilation.Serialization
  ( serialize
  )
  where

import Curios.Compilation.Leb128 (uleb128Builder)
import Curios.Compilation.Utf8 (utf8)
import Data.Word (Word8, Word32)
import Data.List (nub)
import Data.ByteString.Builder (Builder, word8, stringUtf8, int32LE)

import Curios.Compilation.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , DataIdx (..)
  , SecIdx (..)
  , SymIdx (..)

  , NumberType (..)
  , RefType (..)
  , ValueType (..)
  , ResultType (..)
  , FuncType (..)
  , TypeSec (..)

  , Limits (..)
  , TableType (..)
  , MemType (..)
  , Mutability (..)
  , GlobalType (..)
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)

  , Func (..)
  , FuncSec (..)

  , ExportDesc (..)
  , Export (..)
  , ExportSec (..)

  , Elem (..)
  , ElemSec (..)

  , Locals (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Code (..)
  , CodeSec (..)

  , SymKind (..)
  , SymFlag (..)
  , SymInfo (..)
  , SymTable (..)
  , LinkSec (..)

  , Module (..)
  
  , RelocType (..)
  , RelocEntry (..)
  , RelocSec (..)
  )

import Curios.Compilation.Buffer
  ( Buffer (..)
  , byte
  , unsigned
  , unsignedFixed
  , signed
  , signedFixed
  , floatingSingle
  , floatingDouble
  , prependSize
  )

import Curios.Compilation.RelocBuffer
  ( RelocBuffer (..)
  , relocEmpty
  , relocSingleton
  , relocPrependSize
  )

preamble :: Builder
preamble =
  magic <> version where
    magic = stringUtf8 "\0asm"
    version = int32LE 1

class Bufferable a where
  buffered :: a -> Buffer

bufferedVec :: Bufferable a => [a] -> Buffer
bufferedVec values =
  unsigned quantity <> contents where
    quantity = fromIntegral (length values) :: Word32
    contents = mconcat (map buffered values)

instance Bufferable Word8 where
  buffered = byte

bufferedName :: String -> Buffer
bufferedName string =
  bufferedVec (utf8 string)

class RelocBufferable a where
  relocBuffered :: a -> RelocBuffer

relocBufferedVec :: RelocBufferable a => [a] -> RelocBuffer
relocBufferedVec values =
  relocEmpty (unsigned quantity) <> contents where
    quantity = fromIntegral (length values) :: Word32
    contents = mconcat (map relocBuffered values)

class Buildable a where
  built :: a -> Builder

wrapSec :: Word8 -> Buffer -> Builder
wrapSec identifier (Buffer size builder) =
  word8 identifier <> uleb128Builder size <> builder

class RelocBuildable a where
  relocBuilt :: a -> (Builder, [RelocEntry])

wrapRelocSec :: Word8 -> RelocBuffer -> (Builder, [RelocEntry])
wrapRelocSec identifier (RelocBuffer buffer entries) =
  (wrapSec identifier buffer, entries)

instance Bufferable TypeIdx where
  buffered (TypeIdx typeIdx) =
    unsigned typeIdx

instance Bufferable FuncIdx where
  buffered (FuncIdx funcIdx) =
    unsigned funcIdx

instance Bufferable LocalIdx where
  buffered (LocalIdx localIdx) =
    unsigned localIdx

instance Bufferable TableIdx where
  buffered (TableIdx tableIdx) =
    unsigned tableIdx

instance Bufferable MemIdx where
  buffered (MemIdx memIdx) =
    unsigned memIdx

instance Bufferable GlobalIdx where
  buffered (GlobalIdx globalIdx) =
    unsigned globalIdx

instance Bufferable DataIdx where
  buffered (DataIdx dataIdx) =
    unsigned dataIdx

instance Bufferable SecIdx where
  buffered (SecIdx secIdx) =
    unsigned secIdx

instance Bufferable SymIdx where
  buffered (SymIdx symIdx) =
    unsigned symIdx

instance Bufferable NumberType where
  buffered numberType =
    case numberType of
      NtI32 -> byte 0x7F
      NtI64 -> byte 0x7E
      NtF32 -> byte 0x7D
      NtF64 -> byte 0x7C

instance Bufferable RefType where
  buffered refType =
    case refType of
      RfFuncRef -> byte 0x70
      RfExternRef -> byte 0x6F

instance Bufferable ValueType where
  buffered valueType =
    case valueType of
      VtNumberType numberType -> buffered numberType
      VtRefType refType -> buffered refType

instance Bufferable ResultType where
  buffered (ResultType valueTypes) =
    bufferedVec valueTypes

instance Bufferable FuncType where
  buffered (FuncType inputs outputs) =
    byte 0x60 <> buffered inputs <> buffered outputs

instance Buildable TypeSec where
  built (TypeSec types) =
    wrapSec 1 (bufferedVec types)

instance Bufferable Limits where
  buffered limits =
    case limits of
      LmUnbounded minSize ->
        byte 0x00 <> unsigned minSize
      LmBounded minSize maxSize ->
        byte 0x01 <> unsigned minSize <> unsigned maxSize

instance Bufferable TableType where
  buffered (TableType refType limits) =
    buffered refType <> buffered limits

instance Bufferable MemType where
  buffered (MemType limits) =
    buffered limits

instance Bufferable Mutability where
  buffered mutability =
    case mutability of
      MtConst -> byte 0x00
      MtVar -> byte 0x01

instance Bufferable GlobalType where
  buffered (GlobalType valueType mutability) =
    buffered valueType <> buffered mutability

instance Bufferable ImportDesc where
  buffered imptDesc =
    case imptDesc of
      IdFunc typeIdx -> byte 0x00 <> buffered typeIdx
      IdTable tableType -> byte 0x01 <> buffered tableType
      IdMem memType -> byte 0x02 <> buffered memType
      IdGlobal globalType -> byte 0x03 <> buffered globalType

instance Bufferable Import where
  buffered (Import name namespace imptDesc) =
    bufferedName name <> bufferedName namespace <> buffered imptDesc

instance Buildable ImportSec where
  built (ImportSec imports) =
    wrapSec 2 (bufferedVec imports)

instance Bufferable Func where
  buffered (Func typeIdx) =
    buffered typeIdx

instance Buildable FuncSec where
  built (FuncSec funcs) =
    wrapSec 3 (bufferedVec funcs)

instance Bufferable ExportDesc where
  buffered exportDesc =
    case exportDesc of
      EdFunc funcIdx -> byte 0 <> buffered funcIdx
      EdTable tableIdx -> byte 1 <> buffered tableIdx
      EdMem memIdx -> byte 2 <> buffered memIdx
      EdGlobal globalIdx -> byte 3 <> buffered globalIdx

instance Bufferable Export where
  buffered (Export name exportDesc) =
    bufferedName name <> buffered exportDesc

instance Buildable ExportSec where
  built (ExportSec exports) =
    wrapSec 7 (bufferedVec exports)

instance Bufferable Elem where
  buffered (Elem offsetExpr funcIdxs) =
    byte 0x00
      <> let RelocBuffer buffer _ = relocBuffered offsetExpr in buffer
      <> bufferedVec funcIdxs

instance Buildable ElemSec where
  built (ElemSec elems) =
    wrapSec 9 (bufferedVec elems)

instance Bufferable Locals where
  buffered (Locals quantity valueType) =
    unsigned quantity <> buffered valueType

instance Bufferable MemArg where
  buffered (MemArg alignment offset) =
    unsigned alignment <> unsigned offset

instance RelocBufferable Instr where
  relocBuffered instr =
    case instr of
      InI32Const value ->
        relocEmpty (byte 0x41 <> signed value)

      InI32FuncRef value symIdx ->
        relocEmpty (byte 0x41)
          <> relocSingleton (signedFixed 5 value) RlTableIndexSleb symIdx

      InI64Const value ->
        relocEmpty (byte 0x42 <> signed value)

      InF32Const value ->
        relocEmpty (byte 0x43 <> floatingSingle value)

      InF64Const value ->
        relocEmpty (byte 0x44 <> floatingDouble value)

      InI32Load memArg ->
        relocEmpty (byte 0x28 <> buffered memArg)

      InLocalGet localIdx ->
        relocEmpty (byte 0x20 <> buffered localIdx)

      InLocalSet localIdx ->
        relocEmpty (byte 0x21 <> buffered localIdx)

      InLocalTee localIdx ->
        relocEmpty (byte 0x22 <> buffered localIdx)

      InCall (FuncIdx funcIdx) symIdx ->
        relocEmpty (byte 0x10)
          <> relocSingleton (unsignedFixed 5 funcIdx) RlFunctionIndexLeb symIdx

instance RelocBufferable Expr where
  relocBuffered (Expr instrs) =
    mconcat (map relocBuffered instrs) <> relocEmpty (byte 0x0B)

instance RelocBufferable Code where
  relocBuffered (Code locals expr) =
    relocPrependSize (relocEmpty (bufferedVec locals) <> relocBuffered expr)

instance RelocBuildable CodeSec where
  relocBuilt (CodeSec codes) =
    wrapRelocSec 10 (relocBufferedVec codes)

packSymFlags :: [SymFlag] -> Word32
packSymFlags flags =
  sum (map go $ nub flags) where
    go flag =
      case flag of
        SfVisibilityHidden -> 0x04
        SfUndefined -> 0x10
        SfExported -> 0x20
        SfExplicitName -> 0x40

instance Bufferable SymInfo where
  buffered (SymInfo kind flags) =
    case kind of
      SkFunction funcIdx name ->
        byte 0
          <> unsigned (packSymFlags flags)
          <> buffered funcIdx
          <> maybe mempty bufferedName name
        
instance Bufferable SymTable where
  buffered (SymTable symInfos) =
    byte 8 <> prependSize (bufferedVec symInfos)

instance Buildable LinkSec where
  built (LinkSec symTable) =
    wrapSec 0 (name <> version <> buffered symTable) where
      name = bufferedName "linking" 
      version = unsigned (2 :: Word32)

instance Bufferable RelocType where
  buffered relocType =
    case relocType of
      RlFunctionIndexLeb -> byte 0
      RlTableIndexSleb -> byte 1

instance Bufferable RelocEntry where
  buffered (RelocEntry relocType offset symIdx) =
    buffered relocType <> unsigned offset <> buffered symIdx

instance Buildable RelocSec where
  built (RelocSec suffix secIdx entries) =
    wrapSec 0 (name <> buffered secIdx <> bufferedVec entries) where
      name = bufferedName ("reloc." ++ suffix)

instance Buildable Module where
  built modl =
    let
      Module
        { mdTypes = typeSec
        , mdImports = importSec
        , mdFuncs = funcSec
        , mdExports = exportSec
        , mdElems = elemSec
        , mdCodes = codeSec
        , mdLink = linkSec
        }
        = modl
      
      typeBuilder = built typeSec -- 0
      importBuilder = built importSec -- 1
      funcBuilder = built funcSec -- 2
      exportBuilder = built exportSec -- 3
      elemBuilder = built elemSec -- 4
      (codeBuilder, codeRelocs) = relocBuilt codeSec -- 5
      linkBuilder = built linkSec -- 6
      codeRelocBuilder = built (RelocSec "CODE" 5 codeRelocs) -- 7
    in
      preamble
        <> typeBuilder
        <> importBuilder
        <> funcBuilder
        <> exportBuilder
        <> elemBuilder
        <> codeBuilder
        <> linkBuilder
        <> codeRelocBuilder

serialize :: Module -> Builder
serialize modl =
  built modl
