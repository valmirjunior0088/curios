module Inter.Compile
  ( compile
  )
  where

import Prelude hiding (sequence)

import Util (decompose)
import WebAssembly.Syntax.Module (Module (..))
import WebAssembly.Syntax.Instructions (MemArg (..))
import WebAssembly.Syntax.Types (ValType (..), i32, MemType (..), Limits (..))

import Inter.Syntax
  ( unwrap
  , Scope
  , instantiate
  , count
  , free
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Atom (..)
  , Target (..)
  , Expression (..)
  , Sequence (..)
  , consumes
  , Program (..)
  )

import WebAssembly.Construct
  ( MonadConstruct (..)
  , Construct
  , runConstruct
  , importFunc
  , importMem
  , commitFuncTable
  , declareFunc
  , startCode
  , endCode
  , pushCall
  , pushLocal
  , pushLocalTee
  , pushI32Load
  , pushLocalGet
  , pushI32Const
  , pushLocalSet
  , pushI32FuncRef
  , pushI32Store
  , pushCallIndirect
  , pushBlock
  , popBlock
  , pushBr
  , pushI32Eq
  , pushBrIf
  , pushUnreachable
  , pushI32Add
  , pushI32Sub
  , pushI32Mul
  , pushI32DivS
  , pushIf
  , pushIfElse
  , popIfElse
  , pushI32And
  , pushI32Or
  , pushI32LtS
  , pushI32LeS
  , pushI32GtS
  , pushI32GeS
  , pushI32Ne
  , pushF32Const
  , pushF32Load
  , pushF32Store
  , pushF32Add
  , pushF32Sub
  , pushF32Mul
  , pushF32Div
  , pushF32Eq
  , pushF32Ne
  , pushF32Lt
  , pushF32Le
  , pushF32Gt
  , pushF32Ge
  )

import Data.Int (Int32)
import Data.List (elemIndex)
import Control.Monad (forM_, when)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState (..), StateT, execStateT)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (view, (^.), use, (.=), (<>=))

unwrapTarget :: MonadReader Program m => Target -> m Sequence
unwrapTarget Target { block, atoms } = do
  blocks <- view (the @"blocks")

  case lookup block blocks of
    Nothing -> error "unknown block"
    Just scope -> return (instantiate atoms scope)

data EmitState = EmitState
  { nextName :: Int

  , cleanups :: [String]
  , environment :: [String]
  }
  deriving (Generic)

emptyState :: [String] -> EmitState
emptyState environment = EmitState
  { nextName = 0

  , cleanups = []
  , environment
  }

newtype Emit a = Emit (StateT EmitState Compile a)
  deriving (Functor, Applicative, Monad, MonadState EmitState, MonadReader Program)

instance MonadConstruct Emit where
  getConstruct = Emit $ lift getConstruct
  modifyConstruct = Emit . lift . modifyConstruct

runEmit :: Emit a -> [String] -> Compile [String]
runEmit (Emit action) environment =
  view (the @"cleanups") <$> execStateT action (emptyState environment)

freshName :: Emit String
freshName = do
  index <- use (the @"nextName")
  (the @"nextName") .= succ index
  return ("var_" ++ show index)

pushFreshLocal :: ValType -> Emit String
pushFreshLocal valType = do
  name <- freshName
  pushLocal name valType
  return name

pushCleanup :: String -> Emit ()
pushCleanup name = (the @"cleanups") <>= [name]

pushVariableGet :: String -> Emit ()
pushVariableGet name = do
  environment <- use (the @"environment")

  case name `elemIndex` environment of
    Just index -> do
      pushLocalGet "envs"
      pushI32Const (fromIntegral index)
      pushCall "get"
    
    Nothing -> pushLocalGet name

pushAtomGet :: Atom -> Emit ()
pushAtomGet = \case
  Null -> pushI32Const 0
  Variable variable -> pushVariableGet (unwrap variable)

pushPure :: Atom -> Emit String
pushPure atom = do
  name <- pushFreshLocal i32

  pushAtomGet atom
  pushLocalSet name

  return name

pushJump :: Target -> Emit String
pushJump target = do
  name <- pushFreshLocal i32

  mapM_ pushAtomGet (target ^. the @"atoms")
  pushCall ("_block_" ++ target ^. the @"block")
  pushLocalSet name

  return name

pushClosureAlloc :: String -> [Atom] -> Emit String
pushClosureAlloc closure atoms = do
  name <- pushFreshLocal i32

  pushI32Const (fromIntegral $ length atoms)
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"
  pushI32FuncRef ("_closure_" ++ closure)
  pushI32Store MemArg { alignment = 2, offset = 0 }

  forM_ (zip [0..] atoms) $ \(index, atom) -> do
    pushLocalGet name
    pushI32Const index
    pushAtomGet atom
    pushCall "set"

  return name

pushClosureEnter :: Atom -> [Atom] -> Emit String
pushClosureEnter atom atoms = do
  name <- pushFreshLocal i32
  
  pushAtomGet atom
  mapM_ pushAtomGet atoms
  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushCallIndirect (i32 : [i32 | _ <- atoms]) [i32]
  pushLocalSet name

  return name

pushStructAlloc :: [Atom] -> Emit String
pushStructAlloc atoms = do
  name <- pushFreshLocal i32
  
  pushI32Const (fromIntegral $ length atoms)
  pushI32Const 0
  pushCall "new"
  pushLocalSet name

  forM_ (zip [0..] atoms) $ \(index, atom) -> do
    pushLocalGet name
    pushI32Const index
    pushAtomGet atom
    pushCall "set"

  return name

pushStructSelect :: Atom -> Int32 -> Emit String
pushStructSelect atom index = do
  name <- pushFreshLocal i32
  
  pushAtomGet atom
  pushI32Const index
  pushCall "get"
  pushLocalTee name
  pushCall "enter"

  return name

pushInt32Alloc :: Int32 -> Emit String
pushInt32Alloc value = do
  name <- pushFreshLocal i32
  
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"
  pushI32Const value
  pushI32Store MemArg { alignment = 2, offset = 0 }
  
  return name

pushInt32If :: Atom -> Target -> Target -> Emit String
pushInt32If atom truthy falsy = do
  name <- pushFreshLocal i32

  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }

  pushIf "" [] []

  forM_ (truthy ^. the @"atoms") $ \truthyAtom -> do
    pushAtomGet truthyAtom
    pushCall "enter"
  
  mapM_ pushAtomGet (truthy ^. the @"atoms")
  pushCall ("_block_" ++ truthy ^. the @"block")
  pushLocalSet name

  pushIfElse ""

  forM_ (falsy ^. the @"atoms") $ \falsyAtom -> do
    pushAtomGet falsyAtom
    pushCall "enter"
  
  mapM_ pushAtomGet (falsy ^. the @"atoms")
  pushCall ("_block_" ++ falsy ^. the @"block")
  pushLocalSet name

  popIfElse

  return name

pushInt32Match :: Atom -> [(Int32, Target)] -> Emit String
pushInt32Match atom branches = do
  name <- pushFreshLocal i32
  
  pushBlock "exit" [] []

  forM_ [label | (label, _) <- reverse branches] $ \label -> do
    pushBlock ("case-" ++ show label) [] []

  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushLocalSet name

  forM_ [label | (label, _) <- branches] $ \label -> do
    pushLocalGet name
    pushI32Const label
    pushI32Eq
    pushBrIf ("case-" ++ show label)

  pushUnreachable

  forM_ [target | (_, target) <- branches] $ \target -> do
    popBlock

    forM_ (concatMap consumes $ target ^. the @"atoms") $ \variable -> do
      pushVariableGet variable
      pushCall "enter"

    mapM_ pushAtomGet (target ^. the @"atoms")
    pushCall ("_block_" ++ target ^. the @"block")
    pushLocalSet name
    pushBr "exit"

  popBlock

  return name

pushInt32BinOp :: BinOp -> Atom -> Atom -> Emit String
pushInt32BinOp op left right = do
  name <- pushFreshLocal i32

  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"

  pushAtomGet left
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushAtomGet right
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }

  case op of
    Add -> pushI32Add
    Sub -> pushI32Sub
    Mul -> pushI32Mul
    Div -> pushI32DivS

  pushI32Store MemArg { alignment = 2, offset = 0 }

  return name

pushInt32BoolOp :: BoolOp -> Atom -> Atom -> Emit String
pushInt32BoolOp op left right = do
  name <- pushFreshLocal i32
  
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"

  pushAtomGet left
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushAtomGet right
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }

  case op of
    And -> pushI32And
    Or -> pushI32Or
  
  pushI32Store MemArg { alignment = 2, offset = 0 }

  return name

pushInt32CompOp :: CompOp -> Atom -> Atom -> Emit String
pushInt32CompOp op left right = do
  name <- pushFreshLocal i32
  
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"

  pushAtomGet left
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushAtomGet right
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }

  case op of
    Eq -> pushI32Eq
    Ne -> pushI32Ne
    Lt -> pushI32LtS
    Le -> pushI32LeS
    Gt -> pushI32GtS
    Ge -> pushI32GeS
  
  pushI32Store MemArg { alignment = 2, offset = 0 }

  return name

pushFlt32Alloc :: Float -> Emit String
pushFlt32Alloc value = do
  name <- pushFreshLocal i32

  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"
  pushF32Const value
  pushF32Store MemArg { alignment = 2, offset = 0 }

  return name

pushFlt32BinOp :: BinOp -> Atom -> Atom -> Emit String
pushFlt32BinOp op left right = do
  name <- pushFreshLocal i32

  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"

  pushAtomGet left
  pushCall "trunk"
  pushF32Load MemArg { alignment = 2, offset = 0 }
  pushAtomGet right
  pushCall "trunk"
  pushF32Load MemArg { alignment = 2, offset = 0 }

  case op of
    Add -> pushF32Add
    Sub -> pushF32Sub
    Mul -> pushF32Mul
    Div -> pushF32Div

  pushF32Store MemArg { alignment = 2, offset = 0 }

  return name

pushFlt32CompOp :: CompOp -> Atom -> Atom -> Emit String
pushFlt32CompOp op left right = do
  name <- pushFreshLocal i32
  
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"

  pushAtomGet left
  pushCall "trunk"
  pushF32Load MemArg { alignment = 2, offset = 0 }
  pushAtomGet right
  pushCall "trunk"
  pushF32Load MemArg { alignment = 2, offset = 0 }

  case op of
    Eq -> pushF32Eq
    Ne -> pushF32Ne
    Lt -> pushF32Lt
    Le -> pushF32Le
    Gt -> pushF32Gt
    Ge -> pushF32Ge
  
  pushI32Store MemArg { alignment = 2, offset = 0 }

  return name

pushExpression :: Expression -> Emit String
pushExpression = \case
  Pure atom -> pushPure atom
  Jump target -> pushJump target
  ClosureAlloc closure atoms -> pushClosureAlloc closure atoms
  ClosureEnter atom atoms -> pushClosureEnter atom atoms
  StructAlloc atoms -> pushStructAlloc atoms
  StructSelect atom index -> pushStructSelect atom index
  Int32Alloc value -> pushInt32Alloc value
  Int32If atom truthy falsy -> pushInt32If atom truthy falsy
  Int32Match atom branches -> pushInt32Match atom branches
  Int32BinOp op left right -> pushInt32BinOp op left right
  Int32BoolOp op left right -> pushInt32BoolOp op left right
  Int32CompOp op left right -> pushInt32CompOp op left right
  Flt32Alloc value -> pushFlt32Alloc value
  Flt32BinOp op left right -> pushFlt32BinOp op left right
  Flt32CompOp op left right -> pushFlt32CompOp op left right

pushSequence :: Sequence -> Emit ()
pushSequence Sequence { expression, continuation } = do
  environment <- use (the @"environment")
  
  forM_ (decompose $ consumes expression) $ \(variable, variables) -> do
    let 
      inEnvironment = variable `elem` environment
      inVariables = variable `elem` variables
      inContinuation = variable `elem` consumes continuation

    when (inEnvironment || inVariables || inContinuation) $ do
      pushVariableGet variable
      pushCall "enter"
  
  name <- pushExpression expression

  case instantiate [free name] <$> continuation of
    Just sequence -> do
      when (name `notElem` consumes sequence) $ do
        pushCleanup name

      pushSequence sequence

    Nothing -> pushVariableGet name

newtype Compile a = Compile (ReaderT Program Construct a)
  deriving (Functor, Applicative, Monad, MonadReader Program)

instance MonadConstruct Compile where
  getConstruct = Compile $ lift getConstruct
  modifyConstruct = Compile . lift . modifyConstruct

runCompile :: Compile a -> Program -> Construct a
runCompile (Compile action) = runReaderT action

commitSequence :: [String] -> Sequence -> Compile [String]
commitSequence environment sequence = runEmit (pushSequence sequence) environment

prepareBlock :: (String, Scope Sequence) -> Compile ([String], Sequence)
prepareBlock (name, scope) = do
  let
    args = ["arg_" ++ show arg | arg <- [0 .. pred $ count scope]]
    body = instantiate (map free args) scope

  declareFunc ("_block_" ++ name) [(arg, i32) | arg <- args] [i32]

  return (args, body)

commitBlock :: ([String], Sequence) -> Compile ()
commitBlock (args, body) = do
  startCode

  cleanups <- commitSequence [] body

  forM_ (filter (`notElem` consumes body) args ++ cleanups) $ \cleanup -> do
    pushLocalGet cleanup
    pushCall "leave"

  endCode

prepareClosure :: (String, Scope (Scope Target)) -> Compile ([String], [String], Target)
prepareClosure (name, envScope) = do
  let
    envs = ["env_" ++ show env | env <- [0 .. pred $ count envScope]]
    argScope = instantiate (map free envs) envScope 

    args = ["arg_" ++ show arg | arg <- [0 .. pred $ count argScope]]
    target = instantiate (map free args) argScope
  
  declareFunc ("_closure_" ++ name) (("envs", i32) : [(arg, i32) | arg <- args]) [i32]
  
  return (envs, args, target)

commitClosure :: ([String], [String], Target) -> Compile ()
commitClosure (envs, args, target) = do
  startCode

  sequence <- unwrapTarget target
  cleanups <- commitSequence envs sequence

  forM_ (filter (`notElem` consumes sequence) args ++ cleanups) $ \cleanup -> do
    pushLocalGet cleanup
    pushCall "leave"

  endCode

compileProgram :: Compile ()
compileProgram = do
  importMem "env" "__linear_memory" (MemType $ Unbounded 0)
  importFunc "env" "new" [i32, i32] [i32]
  importFunc "env" "enter" [i32] []
  importFunc "env" "leave" [i32] []
  importFunc "env" "set" [i32, i32, i32] []
  importFunc "env" "get" [i32, i32] [i32]
  importFunc "env" "trunk" [i32] [i32]

  preparedBlocks <- mapM prepareBlock =<< view (the @"blocks")
  preparedClosures <- mapM prepareClosure =<< view (the @"closures")
  commitFuncTable (Just ("env", "__indirect_function_table"))
  mapM_ commitBlock preparedBlocks
  mapM_ commitClosure preparedClosures

  declareFunc "_start" [] [i32]
  startCode
  pushLocal "object" i32
  pushCall "_block_start"
  pushLocalTee "object"
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushLocalGet "object"
  pushCall "leave"
  endCode

compile :: Program -> Module
compile = runConstruct . runCompile compileProgram
