module WebAssembly.Serialize
  ( Serialize (..)
  , Build (..)
  , RelocSerialize (..)
  , RelocBuild (..)
  , writeModule
  )
  where

import Prelude hiding (writeFile)

import WebAssembly.Encode.Utf8 (utf8String)
import WebAssembly.Encode.Leb128 (uleb128)

import WebAssembly.Buffer
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

import WebAssembly.Syntax.Conventions
  ( TypeIdx (..)
  , FuncIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , ElemIdx (..)
  , DataIdx (..)
  , LocalIdx (..)
  , LabelIdx (..)
  , SecIdx (..)
  , SymIdx (..)
  , Vec (..)
  , Name (..)
  )

import WebAssembly.Syntax.Types
  ( NumType (..)
  , VecType (..)
  , RefType (..)
  , ValType (..)
  , ResultType (..)
  , FuncType (..)
  , Limits (..)
  , MemType (..)
  , TableType (..)
  , Mut (..)
  , GlobalType (..)
  )

import WebAssembly.Syntax.Instructions
  ( BlockType (..)
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  )

import WebAssembly.Syntax.Module
  ( magic
  , version
  , CustomSec (..)
  , customSecId
  , TypeSec (..)
  , typeSecId
  , ImportDesc (..)
  , Import (..)
  , ImportSec (..)
  , importSecId
  , FuncSec (..)
  , funcSecId
  , Table (..)
  , TableSec (..)
  , tableSecId
  , Mem (..)
  , MemSec (..)
  , memSecId
  , Global (..)
  , GlobalSec (..)
  , globalSecId
  , ExportDesc (..)
  , Export (..)
  , ExportSec (..)
  , exportSecId
  , Start (..)
  , StartSec (..)
  , startSecId
  , ElemKind (..)
  , Elem (..)
  , ElemSec (..)
  , elemSecId
  , Locals (..)
  , Func (..)
  , Code (..)
  , CodeSec (..)
  , codeSecId
  , Data (..)
  , DataSec (..)
  , dataSecId
  , DataCountSec (..)
  , dataCountSecId
  , Module (..)
  )

import WebAssembly.Syntax.LLVM
  ( linkingVersion
  , SymType (..)
  , SymFlags (..)
  , SymInfo (..)
  , LinkingSubsec (..)
  , LinkingSec (..)
  , RelocType (..)
  , RelocEntry (..)
  , RelocSec (..)
  )

import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Builder (Builder, word8, word32LE, stringUtf8, toLazyByteString)
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Monad.State (StateT, execStateT, get, put)
import Data.Generics.Product (the)
import Control.Lens ((^.))

class Serialize a where
  serialize :: a -> Buffer

class Build a where
  build :: a -> Builder

section :: Word8 -> Buffer -> Builder
section identifier Buffer { size, contents } = 
  identifierBuilder <> sizeBuilder <> contents where
    identifierBuilder = word8 identifier
    sizeBuilder = mconcat (map word8 (uleb128 size))

class RelocSerialize a where
  relocSerialize :: a -> RelocBuffer

class RelocBuild a where
  relocBuild :: a -> (Builder, [RelocEntry])

relocSection :: Word8 -> RelocBuffer -> (Builder, [RelocEntry])
relocSection identifier RelocBuffer { target, relocs } =
  (section identifier target, relocs)

instance Serialize Word8 where
  serialize = byte

instance Serialize TypeIdx where
  serialize (TypeIdx idx) = unsigned idx

instance Serialize FuncIdx where
  serialize (FuncIdx idx) = unsigned idx

instance Serialize TableIdx where
  serialize (TableIdx idx) = unsigned idx

instance Serialize MemIdx where
  serialize (MemIdx idx) = unsigned idx

instance Serialize GlobalIdx where
  serialize (GlobalIdx idx) = unsigned idx

instance Serialize ElemIdx where
  serialize (ElemIdx idx) = unsigned idx

instance Serialize DataIdx where
  serialize (DataIdx idx) = unsigned idx

instance Serialize LocalIdx where
  serialize (LocalIdx idx) = unsigned idx

instance Serialize LabelIdx where
  serialize (LabelIdx idx) = unsigned idx

instance Serialize SecIdx where
  serialize (SecIdx idx) = unsigned idx

instance Serialize SymIdx where
  serialize (SymIdx idx) = unsigned idx

instance Serialize a => Serialize (Vec a) where
  serialize (Vec values) = lengthBuffer <> valuesBuffer where
    lengthBuffer = unsigned (fromIntegral $ length values :: Word32)
    valuesBuffer = mconcat (map serialize values)

instance RelocSerialize a => RelocSerialize (Vec a) where
  relocSerialize (Vec values) = relocEmpty lengthBuffer <> valuesBuffer where
    lengthBuffer = unsigned (fromIntegral $ length values :: Word32)
    valuesBuffer = mconcat (map relocSerialize values)

instance Serialize Name where
  serialize (Name name) = serialize (Vec $ utf8String name)

instance Serialize NumType where
  serialize = \case
    I32 -> byte 0x7F
    I64 -> byte 0x7E
    F32 -> byte 0x7D
    F64 -> byte 0x7C

instance Serialize VecType where
  serialize = \case
    V128 -> byte 0x7B

instance Serialize RefType where
  serialize = \case
    FuncRef -> byte 0x70
    ExternRef -> byte 0x6F

instance Serialize ValType where
  serialize = \case
    ValNumType value -> serialize value
    ValVecType value -> serialize value
    ValRefType value -> serialize value

instance Serialize ResultType where
  serialize (ResultType valTypes) = serialize valTypes

instance Serialize FuncType where
  serialize (FuncType inputs outputs) = 
    byte 0x60 <> serialize inputs <> serialize outputs

instance Serialize Limits where
  serialize = \case
    Unbounded lower -> byte 0x00 <> unsigned lower
    Bounded lower upper -> byte 0x01 <> unsigned lower <> unsigned upper

instance Serialize MemType where
  serialize (MemType limits) = serialize limits

instance Serialize TableType where
  serialize (TableType refType limits) = serialize refType <> serialize limits

instance Serialize Mut where
  serialize = \case
    Const -> byte 0x00
    Var -> byte 0x01

instance Serialize GlobalType where
  serialize (GlobalType valType mut) = serialize valType <> serialize mut

instance Serialize BlockType where
  serialize = \case
    BlockEmpty -> byte 0x40
    BlockValType valType -> serialize valType
    BlockTypeIdx typeIdx -> signed (fromIntegral typeIdx :: Int32)

instance Serialize MemArg where
  serialize MemArg { alignment, offset } =
    unsigned alignment <> unsigned offset

instance Serialize Instr where
  serialize = \case
    Unreachable -> byte 0x00

    Nop -> byte 0x01

    Block blockType instrs -> byte 0x02
      <> serialize blockType
      <> mconcat (map serialize instrs)
      <> byte 0x0B
    
    Loop blockType instrs -> byte 0x03
      <> serialize blockType
      <> mconcat (map serialize instrs)
      <> byte 0x0B
    
    Br labelIdx -> byte 0x0C <> serialize labelIdx

    BrIf labelIdx -> byte 0x0D <> serialize labelIdx

    BrTable labelIdxs labelIdx -> byte 0x0E
      <> serialize labelIdxs
      <> serialize labelIdx

    Return -> byte 0x0F

    Call funcIdx _ -> byte 0x10 <> serialize funcIdx

    CallIndirect typeIdx tableIdx -> byte 0x11
      <> serialize typeIdx
      <> serialize tableIdx
    
    Drop -> byte 0x1A
    
    LocalGet localIdx -> byte 0x20 <> serialize localIdx
    LocalSet localIdx -> byte 0x21 <> serialize localIdx
    LocalTee localIdx -> byte 0x22 <> serialize localIdx

    GlobalGet globalIdx _ -> byte 0x23 <> serialize globalIdx
    GlobalSet globalIdx _ -> byte 0x24 <> serialize globalIdx

    I32Load memArg -> byte 0x28 <> serialize memArg
    I32Store memArg -> byte 0x36 <> serialize memArg
    I32Const value -> byte 0x41 <> signed value
    I32Add -> byte 0x6A
    I32Sub -> byte 0x6B
    I32Mul -> byte 0x6C
    I32Eq -> byte 0x46

    I64Load memArg -> byte 0x29 <> serialize memArg
    I64Store memArg -> byte 0x37 <> serialize memArg
    I64Const value -> byte 0x42 <> signed value
    I64Add -> byte 0x7C
    I64Sub -> byte 0x7D
    I64Mul -> byte 0x7E

    F32Load memArg -> byte 0x2A <> serialize memArg
    F32Store memArg -> byte 0x38 <> serialize memArg
    F32Const value -> byte 0x43 <> single value
    F32Add -> byte 0x92
    F32Sub -> byte 0x93
    F32Mul -> byte 0x94

    F64Load memArg -> byte 0x2B <> serialize memArg
    F64Store memArg -> byte 0x39 <> serialize memArg
    F64Const value -> byte 0x44 <> double value
    F64Add -> byte 0xA0
    F64Sub -> byte 0xA1
    F64Mul -> byte 0xA2

    I32FuncRef value _ -> byte 0x41 <> signed value
    I32DataRef value _ _ -> byte 0x41 <> signed value

instance Serialize Expr where
  serialize (Expr instrs) = mconcat (map serialize instrs) <> byte 0x0B

instance Build CustomSec where
  build (CustomSec name bytes) =
    section customSecId (serialize name <> mconcat (map byte bytes))

instance Build TypeSec where
  build (TypeSec funcIdxs) = section typeSecId (serialize funcIdxs)

instance Serialize ImportDesc where
  serialize = \case
    ImportFunc typeIdx -> byte 0x00 <> serialize typeIdx
    ImportTable tableType -> byte 0x01 <> serialize tableType
    ImportMem memType -> byte 0x02 <> serialize memType
    ImportGlobal globalType -> byte 0x03 <> serialize globalType

instance Serialize Import where
  serialize (Import namespace name desc) =
    serialize namespace <> serialize name <> serialize desc

instance Build ImportSec where
  build (ImportSec imports) = section importSecId (serialize imports)

instance Build FuncSec where
  build (FuncSec typeIdxs) = section funcSecId (serialize typeIdxs)

instance Serialize Table where
  serialize (Table tableType) = serialize tableType

instance Build TableSec where
  build (TableSec tables) = section tableSecId (serialize tables)

instance Serialize Mem where
  serialize (Mem memType) = serialize memType

instance Build MemSec where
  build (MemSec mems) = section memSecId (serialize mems)

instance Serialize Global where
  serialize (Global globalType expr) = serialize globalType <> serialize expr

instance Build GlobalSec where
  build (GlobalSec globals) = section globalSecId (serialize globals)

instance Serialize ExportDesc where
  serialize = \case
    ExportFunc funcIdx -> byte 0x00 <> serialize funcIdx
    ExportTable tableIdx -> byte 0x01 <> serialize tableIdx
    ExportMem memIdx -> byte 0x02 <> serialize memIdx
    ExportGlobal globalIdx -> byte 0x03 <> serialize globalIdx

instance Serialize Export where
  serialize (Export name descs) = serialize name <> serialize descs

instance Build ExportSec where
  build (ExportSec exports) = section exportSecId (serialize exports)

instance Serialize Start where
  serialize (Start funcIdx) = serialize funcIdx

instance Build StartSec where
  build (StartSec start) = section startSecId (serialize start)

instance Serialize ElemKind where
  serialize = \case
    ElemFuncRef -> byte 0

instance Serialize Elem where
  serialize (Elem tableIdx expr elemKind funcIdxs) =
    unsigned (2 :: Word32)
      <> serialize tableIdx
      <> serialize expr
      <> serialize elemKind
      <> serialize funcIdxs

instance Build ElemSec where
  build (ElemSec elems) = section elemSecId (serialize elems)

instance Serialize Locals where
  serialize (Locals count valType) = unsigned count <> serialize valType

instance RelocSerialize Instr where
  relocSerialize = \case
    Block blockType instrs -> relocEmpty (byte 0x02)
      <> relocEmpty (serialize blockType)
      <> mconcat (map relocSerialize instrs)
      <> relocEmpty (byte 0x0B)

    Loop blockType instrs -> relocEmpty (byte 0x03)
      <> relocEmpty (serialize blockType)
      <> mconcat (map relocSerialize instrs)
      <> relocEmpty (byte 0x0B)
    
    GlobalGet (GlobalIdx globalIdx) symIdx -> relocEmpty (byte 0x23)
      <> relocSingleton (unsignedFixed 5 globalIdx) R_WASM_GLOBAL_INDEX_LEB symIdx

    GlobalSet (GlobalIdx globalIdx) symIdx -> relocEmpty (byte 0x24)
      <> relocSingleton (unsignedFixed 5 globalIdx) R_WASM_GLOBAL_INDEX_LEB symIdx

    Call (FuncIdx funcIdx) symIdx -> relocEmpty (byte 0x10)
      <> relocSingleton (unsignedFixed 5 funcIdx) R_WASM_FUNCTION_INDEX_LEB symIdx
    
    CallIndirect (TypeIdx typeIdx) tableIdx -> relocEmpty (byte 0x11)
      <> relocSingleton (unsignedFixed 5 typeIdx) R_WASM_TYPE_INDEX_LEB (SymIdx typeIdx)
      <> relocEmpty (serialize tableIdx)

    I32FuncRef value symIdx -> relocEmpty (byte 0x41)
      <> relocSingleton (signedFixed 5 value) R_WASM_TABLE_INDEX_SLEB symIdx
    
    I32DataRef value symIdx addend -> relocEmpty (byte 0x41)
      <> relocSingleton (signedFixed 5 value) (R_WASM_MEMORY_ADDR_SLEB addend) symIdx

    instr -> relocEmpty (serialize instr)

instance RelocSerialize Expr where
  relocSerialize (Expr instrs) =
    mconcat (map relocSerialize instrs) <> relocEmpty (byte 0x0B)

instance RelocSerialize Func where
  relocSerialize (Func locals expr) =
    relocEmpty (serialize locals) <> relocSerialize expr

instance RelocSerialize Code where
  relocSerialize (Code func) = relocPrependSize (relocSerialize func)

instance RelocBuild CodeSec where
  relocBuild (CodeSec codes) = relocSection codeSecId (relocSerialize codes)

instance RelocSerialize Data where
  relocSerialize (Data expr bytes relocs) = relocEmpty (unsigned (0 :: Word32))
    <> relocSerialize expr
    <> RelocBuffer { target = serialize bytes, relocs }

instance RelocBuild DataSec where
  relocBuild (DataSec datas) = relocSection dataSecId (relocSerialize datas)

instance Build DataCountSec where
  build (DataCountSec count) = section dataCountSecId (unsigned count)

pack :: [(Word32, Bool)] -> Word32
pack = sum . map go where
  go (value, flag) = if flag then value else 0

instance Serialize SymFlags where
  serialize flags = unsigned $ pack
    [ (0x01, flags ^. the @"wasm_sym_binding_weak")
    , (0x02, flags ^. the @"wasm_sym_binding_local")
    , (0x04, flags ^. the @"wasm_sym_visibility_hidden")
    , (0x10, flags ^. the @"wasm_sym_undefined")
    , (0x20, flags ^. the @"wasm_sym_exported")
    , (0x40, flags ^. the @"wasm_sym_explicit_name")
    , (0x80, flags ^. the @"wasm_sym_no_strip")
    ]

instance Serialize SymInfo where
  serialize = \case
    SymInfo (SYMTAB_FUNCTION funcIdx maybeName) symFlags -> unsigned (0 :: Word32)
      <> serialize symFlags
      <> serialize funcIdx
      <> maybe mempty serialize maybeName

    SymInfo (SYMTAB_DATA name dataIdx offset size) symFlags -> unsigned (1 :: Word32)
      <> serialize symFlags
      <> serialize name
      <> serialize dataIdx
      <> unsigned offset
      <> unsigned size

    SymInfo (SYMTAB_GLOBAL globalIdx maybeName) symFlags -> unsigned (2 :: Word32)
      <> serialize symFlags
      <> serialize globalIdx
      <> maybe mempty serialize maybeName

instance Serialize LinkingSubsec where
  serialize = \case
    WASM_SYMBOL_TABLE infos -> byte 8 <> prependSize (serialize infos)

instance Build LinkingSec where
  build (LinkingSec subsecs) = build (CustomSec name bytes) where
    name = Name "linking"
    bytes = bytesFrom (unsigned linkingVersion <> mconcat (map serialize subsecs))

instance Serialize RelocEntry where
  serialize = \case
    RelocEntry R_WASM_FUNCTION_INDEX_LEB offset symIdx ->
      byte 0 <> unsigned offset <> serialize symIdx

    RelocEntry R_WASM_TABLE_INDEX_SLEB offset symIdx ->
      byte 1 <> unsigned offset <> serialize symIdx

    RelocEntry R_WASM_TABLE_INDEX_I32 offset symIdx ->
      byte 2 <> unsigned offset <> serialize symIdx

    RelocEntry (R_WASM_MEMORY_ADDR_LEB addend) offset symIdx ->
      byte 3 <> unsigned offset <> serialize symIdx <> signed addend

    RelocEntry (R_WASM_MEMORY_ADDR_SLEB addend) offset symIdx ->
      byte 4 <> unsigned offset <> serialize symIdx <> signed addend

    RelocEntry (R_WASM_MEMORY_ADDR_I32 addend) offset symIdx ->
      byte 5 <> unsigned offset <> serialize symIdx <> signed addend

    RelocEntry R_WASM_TYPE_INDEX_LEB offset symIdx ->
      byte 6 <> unsigned offset <> serialize symIdx

    RelocEntry R_WASM_GLOBAL_INDEX_LEB offset symIdx ->
      byte 7 <> unsigned offset <> serialize symIdx

instance Build RelocSec where
  build (RelocSec target secIdx entries) = build (CustomSec name bytes) where
    name = Name ("reloc." ++ target)
    bytes = bytesFrom (serialize secIdx <> serialize entries)

type ModuleBuilder = StateT SecIdx (Writer Builder)

runModuleBuilder :: ModuleBuilder a -> Builder
runModuleBuilder action = execWriter (execStateT action 0)

tellSec :: Build a => a -> ModuleBuilder ()
tellSec value = do
  let builder = build value

  tell builder

  secIdx <- get
  put (succ secIdx)

tellRelocSec :: RelocBuild a => String -> a -> ModuleBuilder RelocSec
tellRelocSec name value = do
  let (builder, entries) = relocBuild value

  tell builder

  secIdx <- get
  put (succ secIdx)

  return (RelocSec name secIdx $ Vec entries)

instance Build Module where
  build modl = runModuleBuilder $ do
    tell (stringUtf8 magic)
    tell (word32LE version)

    tellSec (TypeSec $ Vec $ modl ^. the @"typeSec")
    tellSec (ImportSec $ Vec $ modl ^. the @"importSec")
    tellSec (FuncSec $ Vec $ modl ^. the @"funcSec")
    tellSec (TableSec $ Vec $ modl ^. the @"tableSec")
    tellSec (MemSec $ Vec $ modl ^. the @"memSec")
    tellSec (GlobalSec $ Vec $ modl ^. the @"globalSec")
    tellSec (ExportSec $ Vec $ modl ^. the @"exportSec")

    case modl ^. the @"startSec" of
      Nothing -> return ()
      Just funcIdx -> tellSec (StartSec $ Start funcIdx)

    tellSec (ElemSec $ Vec $ modl ^. the @"elemSec")
    tellSec (DataCountSec $ fromIntegral $ length $ modl ^. the @"dataSec")
    codeRelocSec <- tellRelocSec "CODE" (CodeSec $ Vec $ modl ^. the @"codeSec")
    dataRelocSec <- tellRelocSec "DATA" (DataSec $ Vec $ modl ^. the @"dataSec")

    tellSec (LinkingSec [WASM_SYMBOL_TABLE $ Vec $ modl ^. the @"linkingSec"])
    tellSec codeRelocSec
    tellSec dataRelocSec

writeModule :: FilePath -> Module -> IO ()
writeModule path modl = writeFile path (toLazyByteString $ build modl)
