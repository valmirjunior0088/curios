{-# LANGUAGE NamedFieldPuns #-}

module Curios.Compilation.Syntax
  ( Syntax
  , runSyntax
  , putFuncImport
  , putTableImport
  , putMemImport
  , putGlobalImport
  , putFuncExport
  , putTableExport
  , putMemExport
  , putGlobalExport
  , putFunc
  , getFunc
  , putSym
  , addFuncImport
  , addFunc
  , addExportedFunc
  , putCode
  , getFuncRef
  , getFuncRefsLength
  , getFuncSym
  , i32Const
  , i32FuncRef
  , i64Const
  , f32Const
  , f64Const
  , i32Load
  , localGet
  , localSet
  , localTee
  , call
  )
  where

import Data.List (findIndex, elemIndex, group)
import Data.Int (Int32, Int64)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState (..), StateT, execStateT, State, evalState)

import Curios.Compilation.Module
  ( TypeIdx (..)
  , FuncIdx (..)
  , LocalIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , SymIdx (..)

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

  , Offset
  , Alignment
  , MemArg (..)
  , Instr (..)
  , Expr (..)
  , Locals (..)
  , Code (..)
  , CodeSec (..)

  , SymKind (..)
  , SymFlag (..)
  , SymInfo (..)
  , SymTable (..)
  , LinkSec (..)

  , Module (..)
  , emptyModule
  )

data ModuleState =
  ModuleState
    { msFuncs :: [String]
    }

emptyModuleState :: ModuleState
emptyModuleState = ModuleState []

type Syntax = StateT Module (State ModuleState)

runSyntax :: Syntax () -> Module
runSyntax action =
  evalState (execStateT action emptyModule) emptyModuleState

getType :: [ValueType] -> [ValueType] -> Syntax TypeIdx
getType inputs outputs = do
  modl @ Module { mdTypes = TypeSec types } <- get

  let funcType = FuncType (ResultType inputs) (ResultType outputs)

  case elemIndex funcType types of
    Nothing -> do
      put modl { mdTypes = TypeSec (types ++ [funcType]) }
      return (fromIntegral $ length types)

    Just typeIdx ->
      return (fromIntegral typeIdx)

putFuncImport :: String -> String -> [ValueType] -> [ValueType] -> Syntax ()
putFuncImport namespace name inputs outputs = do
  Module { mdFuncs = FuncSec funcs } <- get
  when (length funcs > 0) (error "importing funcs invalidates local func indexes")

  modlState @ ModuleState { msFuncs } <- lift get
  (lift . put) modlState { msFuncs = msFuncs ++ [name] }
  
  typeIdx <- getType inputs outputs

  let
    imptDesc = IdFunc typeIdx
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec (impts ++ [impt]) }

putTableImport :: String -> String -> RefType -> Limits -> Syntax ()
putTableImport namespace name refType limits = do
  let
    tableType = TableType refType limits
    imptDesc = IdTable tableType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec (impts ++ [impt]) }

putMemImport :: String -> String -> Limits -> Syntax ()
putMemImport namespace name limits = do
  let
    memType = MemType limits
    imptDesc = IdMem memType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec (impts ++ [impt]) }

putGlobalImport :: String -> String -> ValueType -> Mutability -> Syntax ()
putGlobalImport namespace name valueType mutability = do
  let
    globalType = GlobalType valueType mutability
    imptDesc = IdGlobal globalType
    impt = Import namespace name imptDesc

  modl @ Module { mdImports = ImportSec impts } <- get
  put modl { mdImports = ImportSec (impts ++ [impt]) }

putFuncExport :: String -> FuncIdx -> Syntax ()
putFuncExport name funcIdx = do
  let
    exptDesc = EdFunc funcIdx
    expt = Export name exptDesc

  modl @ Module { mdExports = ExportSec expts } <- get
  put modl { mdExports = ExportSec (expts ++ [expt]) }

putTableExport :: String -> TableIdx -> Syntax ()
putTableExport name tableIdx = do
  let
    exptDesc = EdTable tableIdx
    expt = Export name exptDesc

  modl @ Module { mdExports = ExportSec expts } <- get
  put modl { mdExports = ExportSec (expts ++ [expt]) }

putMemExport :: String -> MemIdx -> Syntax ()
putMemExport name memIdx = do
  let
    exptDesc = EdMem memIdx
    expt = Export name exptDesc

  modl @ Module { mdExports = ExportSec expts } <- get
  put modl { mdExports = ExportSec (expts ++ [expt]) }

putGlobalExport :: String -> GlobalIdx -> Syntax ()
putGlobalExport name globalIdx = do
  let
    exptDesc = EdGlobal globalIdx
    expt = Export name exptDesc

  modl @ Module { mdExports = ExportSec expts } <- get
  put modl { mdExports = ExportSec (expts ++ [expt]) }

putFunc :: String -> [ValueType] -> [ValueType] -> Syntax ()
putFunc name inputs outputs = do
  modlState @ ModuleState { msFuncs } <- lift get
  (lift . put) modlState { msFuncs = msFuncs ++ [name] }

  typeIdx <- getType inputs outputs

  modl @ Module { mdFuncs = FuncSec funcs } <- get
  put modl { mdFuncs = FuncSec (funcs ++ [Func typeIdx]) }

getFunc :: String -> Syntax FuncIdx
getFunc name = do
  ModuleState { msFuncs } <- lift get

  case elemIndex name msFuncs of
    Nothing -> error ("no index associated with func '" ++ name ++ "'")
    Just index -> return (fromIntegral index)

putSym :: SymKind -> [SymFlag] -> Syntax ()
putSym kind flags = do
  modl @ Module { mdLink = LinkSec (SymTable symInfos) } <- get
  
  let
    symInfo = SymInfo kind flags
    symTable = SymTable (symInfos ++ [symInfo])

  put modl { mdLink = LinkSec symTable }

addFuncImport :: String -> String -> [ValueType] -> [ValueType] -> Syntax ()
addFuncImport namespace name inputs outputs = do
  putFuncImport namespace name inputs outputs

  func <- getFunc name
  putSym (SkFunction func $ Just name) [SfUndefined, SfExplicitName]

addFunc :: String -> [ValueType] -> [ValueType] -> Syntax ()
addFunc name inputs outputs = do
  putFunc name inputs outputs

  func <- getFunc name
  putSym (SkFunction func $ Just name) [SfVisibilityHidden]

addExportedFunc :: String -> [ValueType] -> [ValueType] -> Syntax ()
addExportedFunc name inputs outputs = do
  putFunc name inputs outputs

  func <- getFunc name
  putFuncExport name func
  putSym (SkFunction func $ Just name) [SfExported]

buildLocals :: [ValueType] -> [Locals]
buildLocals =
  map go . group where
    go locals = Locals (fromIntegral $ length locals) (head locals)

putCode :: [ValueType] -> [Instr] -> Syntax ()
putCode valueTypes instrs = do
  let
    locals = buildLocals valueTypes
    expr = Expr instrs

  modl @ Module { mdCodes = CodeSec codes } <- get
  put modl { mdCodes = CodeSec (codes ++ [Code locals expr]) }

-- Elem 0 is reserved for the null function pointer
tableOffset :: Int
tableOffset = 1

getFuncRef :: FuncIdx -> Syntax Int32
getFuncRef funcIdx = do
  modl @ Module { mdElems = ElemSec elemSegments } <- get

  case elemSegments of
    [] -> do
      offsetInstr <- i32Const (fromIntegral $ tableOffset)

      let
        offsetExpr = Expr [offsetInstr]
        elemSegment = Elem offsetExpr [funcIdx]
        funcRefIdx = tableOffset + 0

      put modl { mdElems = ElemSec [elemSegment] }
      return (fromIntegral funcRefIdx)
    
    [Elem offsetExpr funcIdxs] ->
      case elemIndex funcIdx funcIdxs of
        Nothing -> do
          let
            elemSegment = Elem offsetExpr (funcIdxs ++ [funcIdx])
            funcRefIdx = tableOffset + length funcIdxs

          put modl { mdElems = ElemSec [elemSegment] }
          return (fromIntegral funcRefIdx)

        Just funcRefIdx ->
          return (fromIntegral $ tableOffset + funcRefIdx)

    _ ->
      error "only a single funcref elem segment is supported"
  
getFuncRefsLength :: Syntax Int
getFuncRefsLength = do
  Module { mdElems = ElemSec elems } <- get

  case elems of
    [] -> return (tableOffset + 0)
    [Elem _ funcIdxs] -> return (tableOffset + length funcIdxs)
    _ -> error "only a single funcref elem segment is supported"

getFuncSym :: FuncIdx -> Syntax SymIdx
getFuncSym funcIdx = do
  Module { mdLink = LinkSec (SymTable symInfos) } <- get

  let
    predicate (SymInfo kind _) =
      case kind of
        SkFunction funcIdx' _ -> funcIdx == funcIdx'

  case findIndex predicate symInfos of
    Nothing -> error "symbol doesn't exist"
    Just symIdx -> return (fromIntegral symIdx)

i32Const :: Int32 -> Syntax Instr
i32Const value =
  return (InI32Const value)

i32FuncRef :: String -> Syntax Instr
i32FuncRef name = do
  func <- getFunc name
  funcRef <- getFuncRef func
  funcSym <- getFuncSym func
  return (InI32FuncRef funcRef funcSym)

i64Const :: Int64 -> Syntax Instr
i64Const value =
  return (InI64Const value)

f32Const :: Float -> Syntax Instr
f32Const value =
  return (InF32Const value)

f64Const :: Double -> Syntax Instr
f64Const value =
  return (InF64Const value)

i32Load :: Alignment -> Offset -> Syntax Instr
i32Load alignment offset =
  return (InI32Load $ MemArg alignment offset)

localGet :: LocalIdx -> Syntax Instr
localGet localIdx =
  return (InLocalGet localIdx)

localSet :: LocalIdx -> Syntax Instr
localSet localIdx =
  return (InLocalGet localIdx)

localTee :: LocalIdx -> Syntax Instr
localTee localIdx =
  return (InLocalTee localIdx)

call :: String -> Syntax Instr
call name = do
  func <- getFunc name
  funcSym <- getFuncSym func
  return (InCall func funcSym)
