module WebAssembly.Construct
  ( MonadConstruct (..)
  , importFunc
  , importTable
  , importMem
  , importGlobal
  , declareFunc
  , declareTable
  , declareMem
  , declareGlobal
  , exportFunc
  , exportTable
  , exportMem
  , exportGlobal
  , declareExportFunc
  , declareExportTable
  , declareExportMem
  , declareExportGlobal
  , setStart
  , commitFuncTable
  , startCode
  , endCode
  , pushLocal
  , pushUnreachable
  , pushNop
  , pushBlock
  , popBlock
  , pushLoop
  , popLoop
  , pushBr
  , pushBrIf
  , pushBrTable
  , pushReturn
  , pushCall
  , pushCallIndirect
  , pushDrop
  , pushLocalGet
  , pushLocalSet
  , pushLocalTee
  , pushGlobalGet
  , pushGlobalSet
  , pushI32Load
  , pushI32Store
  , pushI32Const
  , pushI32Add
  , pushI32Sub
  , pushI32Mul
  , pushI32Eq
  , pushI64Load
  , pushI64Store
  , pushI64Const
  , pushI64Add
  , pushI64Sub
  , pushI64Mul
  , pushF32Load
  , pushF32Store
  , pushF32Const
  , pushF32Add
  , pushF32Sub
  , pushF32Mul
  , pushF64Load
  , pushF64Store
  , pushF64Const
  , pushF64Add
  , pushF64Sub
  , pushF64Mul
  , pushI32FuncRef
  , ConstructT
  , runConstructT
  , Construct
  , runConstruct
  )
  where

import WebAssembly.Syntax.Conventions
  ( TypeIdx
  , FuncIdx
  , TableIdx
  , MemIdx
  , GlobalIdx
  , LocalIdx
  , LabelIdx
  , SymIdx
  , Vec (..)
  , Name (..)
  )

import WebAssembly.Syntax.Types
  ( ValType (..)
  , ResultType (..)
  , FuncType (..)
  , RefType (..)
  , TableType (..)
  , GlobalType (..)
  , Limits (..)
  , MemType (..)
  )

import WebAssembly.Syntax.Module
  ( ImportDesc (..)
  , Import (..)
  , ExportDesc (..)
  , Export (..)
  , Global (..)
  , Table (..)
  , Mem (..)
  , ElemKind (..)
  , Elem (..)
  , Locals (..)
  , Func (..)
  , Code (..)
  , Module (..)
  , emptyModule
  )

import WebAssembly.Syntax.Instructions (BlockType (..), MemArg (..), Instr (..), Expr (..))
import WebAssembly.Syntax.LLVM (SymType (..), SymFlags (..), SymInfo (..))
import Control.Monad (when, unless)
import Control.Monad.State (StateT, execStateT, get, modify)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Int (Int32, Int64)
import Data.List (group)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (Getting, ASetter, view, (.~), (%~), (<>~), mapped, _2, _head)

data ModlState = ModlState
  { nextTypeIdx :: TypeIdx
  , nextFuncIdx :: FuncIdx
  , nextTableIdx :: TableIdx
  , nextMemIdx :: MemIdx
  , nextGlobalIdx :: GlobalIdx
  , nextSymIdx :: SymIdx

  , types :: [(FuncType, TypeIdx)]
  , funcs :: [(String, (FuncIdx, SymIdx))]
  , parameters :: [[String]]
  , tables :: [(String, TableIdx)]
  , mems :: [(String, MemIdx)]
  , globals :: [(String, (GlobalIdx, SymIdx))]
  , funcRefs :: [(String, (Int32, SymIdx))]

  , funcTableIdx :: Maybe TableIdx
  }
  deriving (Show, Generic)

emptyModlState :: ModlState
emptyModlState = ModlState 
  { nextTypeIdx = 0
  , nextFuncIdx = 0
  , nextTableIdx = 0
  , nextMemIdx = 0
  , nextGlobalIdx = 0
  , nextSymIdx = 0

  , types = []
  , funcs = []
  , parameters = []
  , tables = []
  , mems = []
  , globals = []
  , funcRefs = []

  , funcTableIdx = Nothing
  }

data Frame =
  RootFrame |
  BlockFrame BlockType |
  LoopFrame BlockType
  deriving (Show)

data CodeState = CodeState
  { nextLocalIdx :: LocalIdx

  , locals :: [ValType]
  , variables :: [(String, LocalIdx)]

  , frames :: [(Frame, [Instr])]
  , labels :: [(String, LabelIdx)]
  }
  deriving (Show, Generic)

emptyCodeState :: [String] -> CodeState
emptyCodeState names = do
  let
    go (parameters, nextLocalIdx) name =
      ((name, nextLocalIdx) : parameters, succ nextLocalIdx)

    (variables, localIdx) = foldl go ([], 0) names 

  CodeState
    { nextLocalIdx = localIdx

    , locals = []
    , variables = variables

    , frames = [(RootFrame, [])]
    , labels = [("root", 0)]
    }

data ConstructState = ConstructState
  { modl :: Module
  , modlState :: ModlState
  , codeState :: CodeState
  }
  deriving (Show, Generic)

emptyState :: ConstructState
emptyState = ConstructState
  { modl = emptyModule
  , modlState = emptyModlState
  , codeState = emptyCodeState []
  }

class Monad m => MonadConstruct m where
  getConstruct :: m ConstructState
  modifyConstruct :: (ConstructState -> ConstructState) -> m ()

use :: MonadConstruct m => Getting a ConstructState a -> m a
use lens = view lens <$> getConstruct

(.=) :: MonadConstruct m => ASetter ConstructState ConstructState a b -> b -> m ()
(.=) lens value = modifyConstruct (lens .~ value)

(%=) :: MonadConstruct m => ASetter ConstructState ConstructState a b -> (a -> b) -> m ()
(%=) lens action = modifyConstruct (lens %~ action)

(<>=) :: (MonadConstruct m, Semigroup a) => ASetter ConstructState ConstructState a a -> a -> m ()
(<>=) lens value = modifyConstruct (lens <>~ value)

getType :: MonadConstruct m => FuncType -> m TypeIdx
getType funcType = do
  types <- use (the @"modlState" . the @"types")

  case lookup funcType types of
    Nothing -> do
      typeIdx <- use (the @"modlState" . the @"nextTypeIdx")
      (the @"modlState" . the @"nextTypeIdx") .= succ typeIdx

      (the @"modl" . the @"typeSec") <>= [funcType]
      (the @"modlState" . the @"types") %= ((funcType, typeIdx) :)

      return typeIdx
    
    Just typeIdx ->
      return typeIdx

importFunc :: MonadConstruct m => String -> String -> [ValType] -> [ValType] -> m ()
importFunc namespace name inputs outputs = do
  funcSec <- use (the @"modl" . the @"funcSec")

  unless (null funcSec)
    (error "cannot import func after having declared a func")

  typeIdx <- getType
    (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))

  funcIdx <- use (the @"modlState" . the @"nextFuncIdx")
  (the @"modlState" . the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    func = Import (Name namespace) (Name name) (ImportFunc typeIdx)

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = True
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_FUNCTION funcIdx (Just (Name name))) flags
  
  (the @"modl" . the @"importSec") <>= [func]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"funcs") %= ((name, (funcIdx, symIdx)) :)

importTable :: MonadConstruct m => String -> String -> TableType -> m ()
importTable namespace name tableType = do
  tableSec <- use (the @"modl" . the @"tableSec")

  unless (null tableSec)
    (error "cannot import a table after having declared a table")

  tableIdx <- use (the @"modlState" . the @"nextTableIdx")
  (the @"modlState" . the @"nextTableIdx") .= succ tableIdx

  let table = Import (Name namespace) (Name name) (ImportTable tableType)
  
  (the @"modl" . the @"importSec") <>= [table]
  (the @"modlState" . the @"tables") %= ((name, tableIdx) :)

importMem :: MonadConstruct m => String -> String -> MemType -> m ()
importMem namespace name memType = do
  memSec <- use (the @"modl" . the @"memSec")

  unless (null memSec)
    (error "cannot import a mem after having declared a mem")

  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  let mem = Import (Name namespace) (Name name) (ImportMem memType)
  
  (the @"modl" . the @"importSec") <>= [mem]
  (the @"modlState" . the @"mems") %= ((name, memIdx) :)

importGlobal :: MonadConstruct m => String -> String -> GlobalType -> m ()
importGlobal namespace name globalType = do
  globalSec <- use (the @"modl" . the @"globalSec")

  unless (null globalSec)
    (error "cannot import a global after having declared a global")

  globalIdx <- use (the @"modlState" . the @"nextGlobalIdx")
  (the @"modlState" . the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    global = Import (Name namespace) (Name name) (ImportGlobal globalType)

    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = True
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"importSec") <>= [global]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"globals") %= ((name, (globalIdx, symIdx)) :)

declareFunc :: MonadConstruct m => String -> [(String, ValType)] -> [ValType] -> m ()
declareFunc name inputs outputs = do
  typeIdx <- getType
    (FuncType (ResultType (Vec [valType | (_, valType) <- inputs])) (ResultType (Vec outputs)))

  funcIdx <- use (the @"modlState" . the @"nextFuncIdx")
  (the @"modlState" . the @"nextFuncIdx") .= succ funcIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = False
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }

    info = SymInfo (SYMTAB_FUNCTION funcIdx (Just (Name name))) flags
  
  (the @"modl" . the @"funcSec") <>= [typeIdx]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"funcs") %= ((name, (funcIdx, symIdx)) :)
  (the @"modlState" . the @"parameters") <>= [[parameter | (parameter, _) <- inputs]]

declareTable :: MonadConstruct m => String -> TableType -> m ()
declareTable name tableType = do
  tableIdx <- use (the @"modlState" . the @"nextTableIdx")
  (the @"modlState" . the @"nextTableIdx") .= succ tableIdx
  
  (the @"modl" . the @"tableSec") <>= [Table tableType]
  (the @"modlState" . the @"tables") %= ((name, tableIdx) :)

declareMem :: MonadConstruct m => String -> MemType -> m ()
declareMem name memType = do
  memIdx <- use (the @"modlState" . the @"nextMemIdx")
  (the @"modlState" . the @"nextMemIdx") .= succ memIdx

  (the @"modl" . the @"memSec") <>= [Mem memType]
  (the @"modlState" . the @"mems") %= ((name, memIdx) :)

declareGlobal :: MonadConstruct m => String -> GlobalType -> Expr -> m ()
declareGlobal name globalType expr = do
  globalIdx <- use (the @"modlState" . the @"nextGlobalIdx")
  (the @"modlState" . the @"nextGlobalIdx") .= succ globalIdx

  symIdx <- use (the @"modlState" . the @"nextSymIdx")
  (the @"modlState" . the @"nextSymIdx") .= succ symIdx

  let
    flags = SymFlags
      { wasm_sym_binding_weak = False
      , wasm_sym_binding_local = False
      , wasm_sym_visibility_hidden = False
      , wasm_sym_undefined = False
      , wasm_sym_exported = False
      , wasm_sym_explicit_name = True
      , wasm_sym_no_strip = False
      }
    
    info = SymInfo (SYMTAB_GLOBAL globalIdx (Just (Name name))) flags
  
  (the @"modl" . the @"globalSec") <>= [Global globalType expr]
  (the @"modl" . the @"linkingSec") <>= [info]
  (the @"modlState" . the @"globals") %= ((name, (globalIdx, symIdx)) :)

exportFunc :: MonadConstruct m => String -> m ()
exportFunc name = do
  funcs <- use (the @"modlState" . the @"funcs")

  case lookup name funcs of
    Nothing ->
      error ("tried to export unknown function \"" ++ name ++ "\"")

    Just (funcIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportFunc funcIdx)]

exportTable :: MonadConstruct m => String -> m ()
exportTable name = do
  tables <- use (the @"modlState" . the @"tables")

  case lookup name tables of
    Nothing ->
      error ("tried to export unknown table \"" ++ name ++ "\"")

    Just tableIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportTable tableIdx)]

exportMem :: MonadConstruct m => String -> m ()
exportMem name = do
  mems <- use (the @"modlState" . the @"mems")

  case lookup name mems of
    Nothing ->
      error ("tried to export unknown memory \"" ++ name ++ "\"")

    Just memIdx ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportMem memIdx)]

exportGlobal :: MonadConstruct m => String -> m ()
exportGlobal name = do
  globals <- use (the @"modlState" . the @"globals")

  case lookup name globals of
    Nothing ->
      error ("tried to export unknown global \"" ++ name ++ "\"")

    Just (globalIdx, _) ->
      (the @"modl" . the @"exportSec") <>= [Export (Name name) (ExportGlobal globalIdx)]

declareExportFunc :: MonadConstruct m => String -> [(String, ValType)] -> [ValType] -> m ()
declareExportFunc name inputs outputs =
  declareFunc name inputs outputs >> exportFunc name

declareExportTable :: MonadConstruct m => String -> TableType -> m ()
declareExportTable name tableType =
  declareTable name tableType >> exportTable name

declareExportMem :: MonadConstruct m => String -> MemType -> m ()
declareExportMem name memType =
  declareMem name memType >> exportMem name

declareExportGlobal :: MonadConstruct m => String -> GlobalType -> Expr -> m ()
declareExportGlobal name globalType expr =
  declareGlobal name globalType expr >> exportGlobal name

setStart :: MonadConstruct m => String -> m ()
setStart name = do
  funcs <- use (the @"modlState" . the @"funcs")

  case lookup name funcs of
    Nothing -> error ("tried to set unknown function \"" ++ name ++ "\" as start")
    Just (funcIdx, _) -> (the @"modl" . the @"startSec") .= Just funcIdx

commitFuncTable :: MonadConstruct m => Maybe (String, String) -> m ()
commitFuncTable imported = do
  funcTableIdx <- use (the @"modlState" . the @"funcTableIdx")

  when (isJust funcTableIdx)
    (error "cannot commit func table twice")

  tableIdx <- use (the @"modlState" . the @"nextTableIdx")
  (the @"modlState" . the @"nextTableIdx") .= succ tableIdx
  
  funcs <- use (the @"modlState" . the @"funcs")

  (the @"modl" . the @"elemSec") <>=
    [Elem tableIdx (Expr [I32Const 1]) ElemFuncRef (Vec [funcIdx | (_, (funcIdx, _)) <- funcs])]

  (the @"modlState" . the @"funcRefs") .=
    [(name, (funcRef, symIdx)) | (name, (_, symIdx)) <- funcs | funcRef <- [1..]]
  
  (the @"modlState" . the @"funcTableIdx") .= Just tableIdx
  
  let
    size = fromIntegral (1 + length funcs)
    limits = Bounded size size
    tableType = TableType FuncRef limits
  
  case imported of
    Nothing ->
      (the @"modl" . the @"tableSec") <>= [Table tableType]
    
    Just (namespace, name) -> do
      tableSec <- use (the @"modl" . the @"tableSec")

      unless (null tableSec)
        (error "cannot import a func table after having declared a table")

      (the @"modl" . the @"importSec") <>=
        [Import (Name namespace) (Name name) (ImportTable tableType)]

startCode :: MonadConstruct m => m ()
startCode = do
  parameters <- use (the @"modlState" . the @"parameters")
  (the @"modlState" . the @"parameters") .= tail parameters
  
  (the @"codeState") .= emptyCodeState (head parameters)

endCode :: MonadConstruct m => m ()
endCode = do
  CodeState { locals, frames } <- use (the @"codeState")

  case frames of
    [(RootFrame, instrs)] -> do
      let
        build valTypes = Locals (fromIntegral $ length valTypes) (head valTypes)
        built = [build valTypes | valTypes <- group locals]
      
      (the @"modl" . the @"codeSec") <>= [Code (Func (Vec built) (Expr instrs))]

    _ ->
      error "found undelimited frame while trying to emit code"

pushLocal :: MonadConstruct m => String -> ValType -> m ()
pushLocal name valType = do
  localIdx <- use (the @"codeState" . the @"nextLocalIdx")
  (the @"codeState" . the @"nextLocalIdx") .= succ localIdx

  (the @"codeState" . the @"locals") <>= [valType]
  (the @"codeState" . the @"variables") %= ((name, localIdx) :)

pushInstr :: MonadConstruct m => Instr -> m ()
pushInstr instr =
  (the @"codeState" . the @"frames" . _head . _2) <>= [instr]

pushUnreachable :: MonadConstruct m => m ()
pushUnreachable = pushInstr Unreachable

pushNop :: MonadConstruct m => m ()
pushNop = pushInstr Nop

pushFrame :: MonadConstruct m => String -> Frame -> m ()
pushFrame name frame = do
  (the @"codeState" . the @"frames") %= ((frame, []) :)

  (the @"codeState" . the @"labels" . mapped . _2) %= succ
  (the @"codeState" . the @"labels") %= ((name, 0) :)

popFrame :: MonadConstruct m => m (Frame, [Instr])
popFrame = do
  frames <- use (the @"codeState" . the @"frames")
  (the @"codeState" . the @"frames") .= tail frames

  (the @"codeState" . the @"labels") %= tail
  (the @"codeState" . the @"labels" . mapped . _2) %= pred

  return (head frames)

getBlockType :: MonadConstruct m => ([ValType], [ValType]) -> m BlockType
getBlockType = \case
  ([], []) ->
    return BlockEmpty
  
  ([], [valType]) ->
    return (BlockValType valType)
  
  (inputs, outputs) -> do
    typeIdx <- getType
      (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))

    return (BlockTypeIdx typeIdx)

pushBlock :: MonadConstruct m => String -> [ValType] -> [ValType] -> m ()
pushBlock name inputs outputs =
  pushFrame name . BlockFrame =<< getBlockType (inputs, outputs)

popBlock :: MonadConstruct m => m ()
popBlock = popFrame >>= \case
  (BlockFrame blockType, instrs) -> pushInstr (Block blockType instrs)
  _ -> error "tried to pop something that was not a block"

pushLoop :: MonadConstruct m => String -> [ValType] -> [ValType] -> m ()
pushLoop name inputs outputs =
  pushFrame name . LoopFrame =<< getBlockType (inputs, outputs)

popLoop :: MonadConstruct m => m ()
popLoop = popFrame >>= \case
  (LoopFrame blockType, instrs) -> pushInstr (Loop blockType instrs)
  _ -> error "tried to pop something that was not a loop"

getLabel :: MonadConstruct m => String -> m LabelIdx
getLabel name = do
  labels <- use (the @"codeState" . the @"labels")

  case lookup name labels of
    Nothing -> error ("tried to get unknown label \"" ++ name ++ "\"")
    Just labelIdx -> return labelIdx

pushBr :: MonadConstruct m => String -> m ()
pushBr name = pushInstr . Br =<< getLabel name

pushBrIf :: MonadConstruct m => String -> m ()
pushBrIf name = pushInstr . BrIf =<< getLabel name

pushBrTable :: MonadConstruct m => [String] -> String -> m ()
pushBrTable names name =
  pushInstr =<< BrTable <$> (Vec <$> mapM getLabel names) <*> getLabel name

pushReturn :: MonadConstruct m => m ()
pushReturn = pushInstr Return

pushCall :: MonadConstruct m => String -> m ()
pushCall name = do
  funcs <- use (the @"modlState" . the @"funcs")

  case lookup name funcs of
    Nothing -> error ("tried to call unknown function \"" ++ name ++ "\"")
    Just (funcIdx, symIdx) -> pushInstr (Call funcIdx symIdx)

pushCallIndirect :: MonadConstruct m => [ValType] -> [ValType] -> m ()
pushCallIndirect inputs outputs = do
  tableIdx <- use (the @"modlState" . the @"funcTableIdx") >>= \case
    Nothing -> error "tried to call_indirect without having set a func table"
    Just tableIdx -> return tableIdx

  typeIdx <- getType
    (FuncType (ResultType (Vec inputs)) (ResultType (Vec outputs)))
    
  pushInstr (CallIndirect typeIdx tableIdx)

pushDrop :: MonadConstruct m => m ()
pushDrop = pushInstr Drop

getVariable :: MonadConstruct m => String -> m LocalIdx
getVariable name = do
  variables <- use (the @"codeState" . the @"variables")

  case lookup name variables of
    Nothing -> error ("tried to get unknown variable \"" ++ name ++ "\"")
    Just localIdx -> return localIdx

pushLocalGet :: MonadConstruct m => String -> m ()
pushLocalGet name = pushInstr . LocalGet =<< getVariable name

pushLocalSet :: MonadConstruct m => String -> m ()
pushLocalSet name = pushInstr . LocalSet =<< getVariable name

pushLocalTee :: MonadConstruct m => String -> m ()
pushLocalTee name = pushInstr . LocalTee =<< getVariable name

getGlobal :: MonadConstruct m => String -> m (GlobalIdx, SymIdx)
getGlobal name = do
  globals <- use (the @"modlState" . the @"globals")

  case lookup name globals of
    Nothing -> error ("tried to get unknown global \"" ++ name ++ "\"")
    Just (globalIdx, symIdx) -> return (globalIdx, symIdx)

pushGlobalGet :: MonadConstruct m => String -> m ()
pushGlobalGet name =
  pushInstr . uncurry GlobalGet =<< getGlobal name

pushGlobalSet :: MonadConstruct m => String -> m ()
pushGlobalSet name =
  pushInstr . uncurry GlobalSet =<< getGlobal name

pushI32Load :: MonadConstruct m => MemArg -> m ()
pushI32Load memArg = pushInstr (I32Load memArg)

pushI32Store :: MonadConstruct m => MemArg -> m ()
pushI32Store memArg = pushInstr (I32Store memArg)

pushI32Const :: MonadConstruct m => Int32 -> m ()
pushI32Const value = pushInstr (I32Const value)

pushI32Add :: MonadConstruct m => m ()
pushI32Add = pushInstr I32Add

pushI32Sub :: MonadConstruct m => m ()
pushI32Sub = pushInstr I32Sub

pushI32Mul :: MonadConstruct m => m ()
pushI32Mul = pushInstr I32Mul

pushI32Eq :: MonadConstruct m => m ()
pushI32Eq = pushInstr I32Eq

pushI64Load :: MonadConstruct m => MemArg -> m ()
pushI64Load memArg = pushInstr (I64Load memArg)

pushI64Store :: MonadConstruct m => MemArg -> m ()
pushI64Store memArg = pushInstr (I64Store memArg)

pushI64Const :: MonadConstruct m => Int64 -> m ()
pushI64Const value = pushInstr (I64Const value)

pushI64Add :: MonadConstruct m => m ()
pushI64Add = pushInstr I64Add

pushI64Sub :: MonadConstruct m => m ()
pushI64Sub = pushInstr I64Sub

pushI64Mul :: MonadConstruct m => m ()
pushI64Mul = pushInstr I64Mul

pushF32Load :: MonadConstruct m => MemArg -> m ()
pushF32Load memArg = pushInstr (F32Load memArg)

pushF32Store :: MonadConstruct m => MemArg -> m ()
pushF32Store memArg = pushInstr (F32Store memArg)

pushF32Const :: MonadConstruct m => Float -> m ()
pushF32Const value = pushInstr (F32Const value)

pushF32Add :: MonadConstruct m => m ()
pushF32Add = pushInstr F32Add

pushF32Sub :: MonadConstruct m => m ()
pushF32Sub = pushInstr F32Sub

pushF32Mul :: MonadConstruct m => m ()
pushF32Mul = pushInstr F32Mul

pushF64Load :: MonadConstruct m => MemArg -> m ()
pushF64Load memArg = pushInstr (F64Load memArg)

pushF64Store :: MonadConstruct m => MemArg -> m ()
pushF64Store memArg = pushInstr (F64Store memArg)

pushF64Const :: MonadConstruct m => Double -> m ()
pushF64Const value = pushInstr (F64Const value)

pushF64Add :: MonadConstruct m => m ()
pushF64Add = pushInstr F64Add

pushF64Sub :: MonadConstruct m => m ()
pushF64Sub = pushInstr F64Sub

pushF64Mul :: MonadConstruct m => m ()
pushF64Mul = pushInstr F64Mul

pushI32FuncRef :: MonadConstruct m => String -> m ()
pushI32FuncRef name = do
  funcRefs <- use (the @"modlState" . the @"funcRefs")

  case lookup name funcRefs of
    Nothing -> error ("tried to create func ref from unknown function \"" ++ name ++ "\"")
    Just (funcRef, symIdx) -> pushInstr (I32FuncRef funcRef symIdx)

newtype ConstructT m a =
  ConstructT (StateT ConstructState m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadConstruct (ConstructT m) where
  getConstruct = ConstructT get
  modifyConstruct = ConstructT . modify

runConstructT :: Monad m => ConstructT m a -> m Module
runConstructT (ConstructT action) =
  view (the @"modl") <$> execStateT action emptyState

type Construct = ConstructT Identity

runConstruct :: Construct a -> Module
runConstruct = runIdentity . runConstructT
