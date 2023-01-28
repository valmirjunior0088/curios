module Intermediate.Compile
  ( compile
  )
  where

import Intermediate.Syntax
  ( BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Atom (..)
  , Expression (..)
  , Sequence (..)
  , Closure (..)
  , Block (..)
  , Program (..)
  , mentions
  , consumes
  )

import WebAssembly.Construct
  ( MonadConstruct (..)
  , Construct
  , runConstruct
  , importFunc
  , declareFunc
  , importMem
  , commitFuncTable
  , startCode
  , endCode
  , pushUnreachable
  , pushBlock
  , popBlock
  , pushIf
  , pushIfElse
  , popIfElse
  , pushBr
  , pushBrIf
  , pushCall
  , pushCallIndirect
  , pushLocal
  , pushLocalGet
  , pushLocalSet
  , pushLocalTee
  , pushI32Load
  , pushI32Store
  , pushI32Const
  , pushI32Add
  , pushI32Sub
  , pushI32Mul
  , pushI32And
  , pushI32Or
  , pushI32DivS
  , pushI32Eq
  , pushI32Ne
  , pushI32LtS
  , pushI32LeS
  , pushI32GtS
  , pushI32GeS
  , pushF32Load
  , pushF32Store
  , pushF32Const
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
  , pushI32FuncRef
  )

import WebAssembly.Syntax.Types (i32, Limits (..), MemType (..))
import WebAssembly.Syntax.Instructions (MemArg (..))
import WebAssembly.Syntax.Module (Module)
import Data.List (elemIndex)
import Data.Int (Int32)
import Control.Monad (forM_, unless)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState, StateT, evalStateT)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (<>=))

decompose :: [a] -> [(a, [a])]
decompose = \case
  [] -> []
  value : rest -> (value, rest) : decompose rest

data EmitState = EmitState
  { environment :: [String]
  , cleanups :: [String]
  }
  deriving (Generic)

newtype Emit a =
  Emit (StateT EmitState Construct a)
  deriving (Functor, Applicative, Monad, MonadState EmitState)

instance MonadConstruct Emit where
  getConstruct = Emit $ lift getConstruct
  modifyConstruct = Emit . lift . modifyConstruct

runEmit :: Emit a -> EmitState -> Construct a
runEmit (Emit action) = evalStateT action

pushEnvironmentalGet :: String -> Emit ()
pushEnvironmentalGet name = do
  pushLocalGet "self"

  environment <- use (the @"environment")

  case name `elemIndex` environment of
    Nothing -> error "no such environmental variable"
    Just index -> pushI32Const (fromIntegral index)

  pushCall "get"

pushAtomGet :: Atom -> Emit ()
pushAtomGet = \case
  Environmental name -> pushEnvironmentalGet name
  Local name -> pushLocalGet name
  Null -> pushI32Const 0

pushCleanup :: String -> Emit ()
pushCleanup name = (the @"cleanups") <>= [name]

commitCleanups :: Emit ()
commitCleanups = do
  cleanups <- use (the @"cleanups")

  forM_ cleanups $ \cleanup -> do
    pushLocalGet cleanup
    pushCall "leave"

pushInt32Alloc :: String -> Int32 -> Emit ()
pushInt32Alloc name value = do
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"
  pushI32Const value
  pushI32Store MemArg { alignment = 2, offset = 0 }

pushInt32Match :: String -> Atom -> [(Int32, String, [Atom])] -> Emit ()
pushInt32Match name atom branches = do
  pushBlock "exit" [] []

  forM_ [label | (label, _, _) <- reverse branches] $ \label -> do
    pushBlock ("case-" ++ show label) [] []

  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushLocalSet name

  forM_ [label | (label, _, _) <- branches] $ \label -> do
    pushLocalGet name
    pushI32Const label
    pushI32Eq
    pushBrIf ("case-" ++ show label)

  pushUnreachable

  forM_ [(block, arguments) | (_, block, arguments) <- branches] $ \(block, arguments) -> do
    popBlock

    forM_ arguments $ \argument -> do
      pushAtomGet argument
      pushCall "enter"

    pushBlockCall name block arguments
    pushBr "exit"

  popBlock

pushInt32If :: String -> Atom -> (String, [Atom]) -> (String, [Atom]) -> Emit ()
pushInt32If name atom (truthyBlock, truthyArguments) (falsyBlock, falsyArguments) = do
  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }

  pushIf "" [] []

  forM_ truthyArguments $ \truthyArgument -> do
    pushAtomGet truthyArgument
    pushCall "enter"

  pushBlockCall name truthyBlock truthyArguments

  pushIfElse ""

  forM_ falsyArguments $ \falsyArgument -> do
    pushAtomGet falsyArgument
    pushCall "enter"

  pushBlockCall name falsyBlock falsyArguments

  popIfElse

pushInt32BinOp :: String -> BinOp -> Atom -> Atom -> Emit ()
pushInt32BinOp name op left right = do
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

pushInt32BoolOp :: String -> BoolOp -> Atom -> Atom -> Emit ()
pushInt32BoolOp name op left right = do
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

pushInt32CompOp :: String -> CompOp -> Atom -> Atom -> Emit ()
pushInt32CompOp name op left right = do
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

pushFlt32Alloc :: String -> Float -> Emit ()
pushFlt32Alloc name value = do
  pushI32Const 0
  pushI32Const 4
  pushCall "new"
  pushLocalTee name
  pushCall "trunk"
  pushF32Const value
  pushF32Store MemArg { alignment = 2, offset = 0 }

pushFlt32BinOp :: String -> BinOp -> Atom -> Atom -> Emit ()
pushFlt32BinOp name op left right = do
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

pushFlt32CompOp :: String -> CompOp -> Atom -> Atom -> Emit ()
pushFlt32CompOp name op left right = do
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

pushPure :: String -> Atom -> Emit ()
pushPure name atom = do
  pushAtomGet atom
  pushLocalSet name

pushBlockCall :: String -> String -> [Atom] -> Emit ()
pushBlockCall name function parameters = do
  mapM_ pushAtomGet parameters
  pushCall ("_block_" ++ function)
  pushLocalSet name

pushClosureAlloc :: String -> String -> [Atom] -> Emit ()
pushClosureAlloc name closure atoms = do
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

pushClosureEnter :: String -> Atom -> [Atom] -> Emit ()
pushClosureEnter name atom parameters = do
  pushAtomGet atom
  mapM_ pushAtomGet parameters
  pushAtomGet atom
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  pushCallIndirect (i32 : [i32 | _ <- parameters]) [i32]
  pushLocalSet name

pushStructAlloc :: String -> [Atom] -> Emit ()
pushStructAlloc name atoms = do
  pushI32Const (fromIntegral $ length atoms)
  pushI32Const 0
  pushCall "new"
  pushLocalSet name

  forM_ (zip [0..] atoms) $ \(index, atom) -> do
    pushLocalGet name
    pushI32Const index
    pushAtomGet atom
    pushCall "set"

pushStructSelect :: String -> Atom -> Int32 -> Emit ()
pushStructSelect name atom index = do
  pushAtomGet atom
  pushI32Const index
  pushCall "get"
  pushLocalTee name
  pushCall "enter"

pushExpression :: String -> Expression -> Emit ()
pushExpression name = \case
  Int32Alloc value -> pushInt32Alloc name value
  Int32Match atom branches -> pushInt32Match name atom branches
  Int32If atom truthy falsy -> pushInt32If name atom truthy falsy
  Int32BinOp op one other -> pushInt32BinOp name op one other
  Int32BoolOp op one other -> pushInt32BoolOp name op one other
  Int32CompOp op one other -> pushInt32CompOp name op one other
  Flt32Alloc value -> pushFlt32Alloc name value
  Flt32BinOp op one other -> pushFlt32BinOp name op one other
  Flt32CompOp op one other -> pushFlt32CompOp name op one other
  Pure atom -> pushPure name atom
  BlockCall function atoms -> pushBlockCall name function atoms
  ClosureAlloc closure atoms -> pushClosureAlloc name closure atoms
  ClosureEnter atom atoms -> pushClosureEnter name atom atoms
  StructAlloc atoms -> pushStructAlloc name atoms
  StructSelect atom index -> pushStructSelect name atom index

pushSequence :: Sequence -> Emit Atom
pushSequence = \case
  Tail body -> do
    forM_ (decompose $ mentions body) $ \case
      (Environmental variable, _) -> do
        pushEnvironmentalGet variable
        pushCall "enter"

      (Local variable, rest) | Local variable `elem` rest  -> do
        pushLocalGet variable
        pushCall "enter"

      (_, _) ->
        return ()

    case body of
      Pure atom -> do
        return atom

      _ -> do
        pushLocal "return" i32
        pushExpression "return" body
        return (Local "return")

  Bind name body rest -> do
    unless (rest `consumes` name) (pushCleanup name)

    forM_ (decompose $ mentions body) $ \case
      (Environmental variable, _) -> do
        pushEnvironmentalGet variable
        pushCall "enter"

      (Local variable, atoms) | Local variable `elem` atoms || rest `consumes` variable  -> do
        pushLocalGet variable
        pushCall "enter"

      (_, _) ->
        return ()

    pushLocal name i32
    pushExpression name body
    pushSequence rest

declareClosure :: (String, Closure) -> Construct ()
declareClosure (name, Closure { parameters }) = do
  let
    inputs = ("self", i32) : [(parameter, i32) | parameter <- parameters]
    outputs = [i32]

  declareFunc ("_closure_" ++ name) inputs outputs

declareBlock :: (String, Block) -> Construct ()
declareBlock (name, Block { parameters }) = do
  let
    inputs = [(parameter, i32) | parameter <- parameters]
    outputs = [i32]

  declareFunc ("_block_" ++ name) inputs outputs

defineClosure :: Closure -> Construct ()
defineClosure Closure { environment, parameters, body } = do
  let
    state = EmitState environment (filter (not . consumes body) parameters)
    emitter = do atom <- pushSequence body; commitCleanups; pushAtomGet atom

  startCode >> runEmit emitter state >> endCode

defineBlock :: Block -> Construct ()
defineBlock Block { parameters, body } = do
  let
    state = EmitState [] (filter (not . consumes body) parameters)
    emitter = do atom <- pushSequence body; commitCleanups; pushAtomGet atom

  startCode >> runEmit emitter state >> endCode

compile :: Program -> Module
compile Program { closures, blocks } = runConstruct $ do
  importFunc "env" "new" [i32, i32] [i32]
  importFunc "env" "enter" [i32] []
  importFunc "env" "leave" [i32] []
  importFunc "env" "set" [i32, i32, i32] []
  importFunc "env" "get" [i32, i32] [i32]
  importFunc "env" "trunk" [i32] [i32]

  forM_ closures declareClosure
  forM_ blocks declareBlock

  importMem "env" "__linear_memory" (MemType $ Unbounded 0)
  commitFuncTable (Just ("env", "__indirect_function_table"))

  forM_ [closure | (_, closure) <- closures] defineClosure
  forM_ [block | (_, block) <- blocks] defineBlock

  declareFunc "_start" [] [i32]
  startCode
  pushCall "_block_start"
  pushCall "trunk"
  pushI32Load MemArg { alignment = 2, offset = 0 }
  endCode
