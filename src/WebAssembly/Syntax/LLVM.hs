module WebAssembly.Syntax.LLVM
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
  where

import WebAssembly.Syntax.Conventions
  ( FuncIdx
  , DataIdx
  , GlobalIdx
  , SecIdx
  , SymIdx
  , Vec
  , Name
  )

import Data.Int (Int32)
import Data.Word (Word32)
import GHC.Generics (Generic)

linkingVersion :: Word32
linkingVersion = 2

data SymType =
  SYMTAB_FUNCTION FuncIdx (Maybe Name) |
  SYMTAB_DATA Name DataIdx Word32 Word32 |
  SYMTAB_GLOBAL GlobalIdx (Maybe Name)
  deriving (Show)

data SymFlags = SymFlags
  { wasm_sym_binding_weak :: Bool
  , wasm_sym_binding_local :: Bool
  , wasm_sym_visibility_hidden :: Bool
  , wasm_sym_undefined :: Bool
  , wasm_sym_exported :: Bool
  , wasm_sym_explicit_name :: Bool
  , wasm_sym_no_strip :: Bool
  }
  deriving (Show, Generic)

data SymInfo =
  SymInfo SymType SymFlags
  deriving (Show)

newtype LinkingSubsec =
  WASM_SYMBOL_TABLE (Vec SymInfo)
  deriving (Show)

newtype LinkingSec =
  LinkingSec [LinkingSubsec]
  deriving (Show)

data RelocType =
  R_WASM_FUNCTION_INDEX_LEB |
  R_WASM_TABLE_INDEX_SLEB |
  R_WASM_TABLE_INDEX_I32 |
  R_WASM_MEMORY_ADDR_LEB Int32 |
  R_WASM_MEMORY_ADDR_SLEB Int32 |
  R_WASM_MEMORY_ADDR_I32 Int32 |
  R_WASM_TYPE_INDEX_LEB |
  R_WASM_GLOBAL_INDEX_LEB
  deriving (Show)

data RelocEntry =
  RelocEntry RelocType Word32 SymIdx
  deriving (Show)

data RelocSec =
  RelocSec String SecIdx (Vec RelocEntry)
  deriving (Show)
