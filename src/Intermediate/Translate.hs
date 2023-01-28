module Intermediate.Translate
  ( translate
  )
  where

import Core.Syntax (Variable (..), Scope, Term, open, free)
import qualified Core.Syntax as Core

import Core.Bindings (Bindings)
import qualified Core.Bindings as Core

import Intermediate.Syntax
  ( BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Atom (..)
  , Expression (..)
  , Sequence (..)
  , bind
  , Block (..)
  , Closure (..)
  , Program (..)
  , emptyProgram
  )

import Util ((!!!))
import Data.List (nub)
import Data.Int (Int32)
import Control.Monad (forM)
import Control.Monad.State (MonadState (..), State, execState)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT, asks)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (^.), (.=), (<>=), (%=))

data TranslateState = TranslateState
  { program :: Program

  , nextLabel :: Int32
  , labels :: [(String, Int32)]

  , prefix :: String
  , nextName :: Int32
  , nextClosure :: Int32
  , nextBlock :: Int32
  }
  deriving (Show, Generic)

emptyState :: TranslateState
emptyState = TranslateState
  { program = emptyProgram

  , nextLabel = 0
  , labels = []

  , prefix = ""
  , nextName = 0
  , nextClosure = 0
  , nextBlock = 0
  }

newtype Translate a =
  Translate (ReaderT [(String, Atom)] (State TranslateState) a)
  deriving (Functor, Applicative, Monad, MonadReader [(String, Atom)], MonadState TranslateState)

runTranslate :: Translate a -> TranslateState
runTranslate (Translate action) = execState (runReaderT action []) emptyState

withDictionary :: [(String, Atom)] -> Translate a -> Translate a
withDictionary dictionary = local (const dictionary)

pushDictionary :: [(String, Atom)] -> Translate a -> Translate a
pushDictionary dictionary = local (++ dictionary)

translateLocal :: String -> Translate Atom
translateLocal name = asks (name !!!)

pushClosure :: String -> [String] -> [String] -> Sequence -> Translate ()
pushClosure name environment parameters body =
  (the @"program" . the @"closures") <>= [(name, Closure { environment, parameters, body })]

pushBlock :: String -> [String] -> Sequence -> Translate ()
pushBlock name parameters body =
  (the @"program" . the @"blocks") <>= [(name, Block { parameters, body })]

getLabel :: String -> Translate Int32
getLabel name = do
  labels <- use (the @"labels")

  case lookup name labels of
    Nothing -> do
      label <- use (the @"nextLabel")
      (the @"nextLabel") .= succ label

      (the @"labels") %= ((name, label) :)

      return label

    Just label ->
      return label

freshName :: Translate String
freshName = do
  name <- use (the @"nextName")
  (the @"nextName") .= succ name

  return (show name)

freshClosure :: Translate String
freshClosure = do
  prefix <- use (the @"prefix")

  closure <- use (the @"nextClosure")
  (the @"nextClosure") .= succ closure

  return (prefix ++ "_" ++ show closure)

freshBlock :: Translate String
freshBlock = do
  prefix <- use (the @"prefix")

  block <- use (the @"nextBlock")
  (the @"nextBlock") .= succ block

  return (prefix ++ "_" ++ show block)

translateNull :: Translate Sequence
translateNull = return (Tail $ Pure Null)

translateVariable :: Variable -> Translate Sequence
translateVariable = \case
  Global name -> return (Tail $ BlockCall name [])
  LocalFree name -> Tail . Pure <$> translateLocal name
  _ -> error "bound variable -- should not happen"

translateFunction :: Scope Term -> Translate Sequence
translateFunction scope = do
  parameter <- freshName
  closure <- freshClosure

  let
    output = open parameter scope
    variables = filter (/= parameter) (nub $ free output)

    environmentals = [(variable, Environmental variable) | variable <- variables]
    locals = [(parameter, Local parameter)]
    dictionary = environmentals ++ locals

  body <- withDictionary dictionary (translateTerm output)
  pushClosure closure variables [parameter] body

  environment <- mapM translateLocal variables
  return (Tail $ ClosureAlloc closure environment)

translateApply :: Term -> Term -> Translate Sequence
translateApply function argument = do
  functionSequence <- translateTerm function
  functionName <- freshName
  argumentSequence <- translateTerm argument
  argumentName <- freshName

  let
    bindFunction = bind functionName functionSequence
    bindArgument = bind argumentName argumentSequence
    tailSequence = Tail (ClosureEnter (Local functionName) [Local argumentName])

  return (bindFunction $ bindArgument tailSequence)

translatePair :: Term -> Term -> Translate Sequence
translatePair left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (StructAlloc [Local leftName, Local rightName])

  return (bindLeft $ bindRight tailSequence)

translateSplit :: Term -> Scope (Scope Term) -> Translate Sequence
translateSplit scrutinee scope = do
  scrutineeSequence <- translateTerm scrutinee
  scrutineeName <- freshName
  leftName <- freshName
  rightName <- freshName

  let
    dictionary = [(leftName, Local leftName), (rightName, Local rightName)]
    output = open rightName (open leftName scope)

    bindScrutinee = bind scrutineeName scrutineeSequence
    bindLeft = Bind leftName (StructSelect (Local scrutineeName) 0)
    bindRight = Bind rightName (StructSelect (Local scrutineeName) 1)

  body <- pushDictionary dictionary (translateTerm output)
  return (bindScrutinee $ bindLeft $ bindRight body)

translateLabel :: String -> Translate Sequence
translateLabel name = Tail . Int32Alloc <$> getLabel name

translateMatch :: Term -> [(String, Term)] -> Translate Sequence
translateMatch scrutinee branches = do
  scrutineeSequence <- translateTerm scrutinee
  scrutineeName <- freshName

  branchesBlockCalls <- forM branches $ \(name, branch) -> do
    let
      branchVariables = nub (free branch)
      branchDictionary = [(variable, Local variable) | variable <- branchVariables]

    branchBlock <- freshBlock
    branchSequence <- withDictionary branchDictionary (translateTerm branch)
    pushBlock branchBlock branchVariables branchSequence

    branchLabel <- getLabel name
    branchArguments <- mapM translateLocal branchVariables
    return (branchLabel, branchBlock, branchArguments)

  let
    bindScrutinee = bind scrutineeName scrutineeSequence
    tailSequence = Tail (Int32Match (Local scrutineeName) branchesBlockCalls)

  return (bindScrutinee tailSequence)

translateInt32 :: Int32 -> Translate Sequence
translateInt32 value = return (Tail $ Int32Alloc value)

translateInt32If :: Term -> Term -> Term -> Translate Sequence
translateInt32If scrutinee truthy falsy = do
  scrutineeSequence <- translateTerm scrutinee
  scrutineeName <- freshName

  let
    truthyVariables = nub (free truthy)
    truthyDictionary = [(variable, Local variable) | variable <- truthyVariables]

  truthyBlock <- freshBlock
  truthySequence <- withDictionary truthyDictionary (translateTerm truthy)
  truthyArguments <- mapM translateLocal truthyVariables
  pushBlock truthyBlock truthyVariables truthySequence

  let
    falsyVariables = nub (free falsy)
    falsyDictionary = [(variable, Local variable) | variable <- falsyVariables]

  falsyBlock <- freshBlock
  falsySequence <- withDictionary falsyDictionary (translateTerm falsy)
  falsyArguments <- mapM translateLocal falsyVariables
  pushBlock falsyBlock falsyVariables falsySequence

  let
    bindScrutinee = bind scrutineeName scrutineeSequence
    truthyBlockCall = (truthyBlock, truthyArguments)
    falsyBlockCall = (falsyBlock, falsyArguments)
    tailSequence = Tail (Int32If (Local scrutineeName) truthyBlockCall falsyBlockCall)

  return (bindScrutinee tailSequence)

translateInt32BinOp :: BinOp -> Term -> Term -> Translate Sequence
translateInt32BinOp op left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (Int32BinOp op (Local leftName) (Local rightName))

  return (bindLeft $ bindRight tailSequence)

translateInt32BoolOp :: BoolOp -> Term -> Term -> Translate Sequence
translateInt32BoolOp op left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (Int32BoolOp op (Local leftName) (Local rightName))

  return (bindLeft $ bindRight tailSequence)

translateInt32CompOp :: CompOp -> Term -> Term -> Translate Sequence
translateInt32CompOp op left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (Int32CompOp op (Local leftName) (Local rightName))

  return (bindLeft $ bindRight tailSequence)

translateFlt32 :: Float -> Translate Sequence
translateFlt32 value = return (Tail $ Flt32Alloc value)

translateFlt32BinOp :: BinOp -> Term -> Term -> Translate Sequence
translateFlt32BinOp op left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (Flt32BinOp op (Local leftName) (Local rightName))

  return (bindLeft $ bindRight tailSequence)

translateFlt32CompOp :: CompOp -> Term -> Term -> Translate Sequence
translateFlt32CompOp op left right = do
  leftSequence <- translateTerm left
  leftName <- freshName
  rightSequence <- translateTerm right
  rightName <- freshName

  let
    bindLeft = bind leftName leftSequence
    bindRight = bind rightName rightSequence
    tailSequence = Tail (Flt32CompOp op (Local leftName) (Local rightName))

  return (bindLeft $ bindRight tailSequence)

translateTerm :: Term -> Translate Sequence
translateTerm = \case
  Core.Variable _ variable -> translateVariable variable
  Core.Type {} -> translateNull
  Core.FunctionType {} -> translateNull
  Core.Function _ scope -> translateFunction scope
  Core.Apply _ function argument -> translateApply function argument
  Core.PairType {} -> translateNull
  Core.Pair _ left right -> translatePair left right
  Core.Split _ scrutinee scope -> translateSplit scrutinee scope
  Core.LabelType {} -> translateNull
  Core.Label _ name -> translateLabel name
  Core.Match _ scrutinee branches -> translateMatch scrutinee branches
  Core.Int32Type {} -> translateNull
  Core.Int32 _ value -> translateInt32 value
  Core.Int32If _ scrutinee truthy falsy -> translateInt32If scrutinee truthy falsy
  Core.Int32BinOp _ op left right -> translateInt32BinOp op left right
  Core.Int32BoolOp _ op left right -> translateInt32BoolOp op left right
  Core.Int32CompOp _ op left right -> translateInt32CompOp op left right
  Core.Flt32Type {} -> translateNull
  Core.Flt32 _ value -> translateFlt32 value
  Core.Flt32BinOp _ op left right -> translateFlt32BinOp op left right
  Core.Flt32CompOp _ op left right -> translateFlt32CompOp op left right

pushDefinition :: (String, Term) -> Translate ()
pushDefinition (name, term) = do
  (the @"prefix") .= name
  (the @"nextName") .= 0
  (the @"nextBlock") .= 0
  (the @"nextClosure") .= 0
  pushBlock name [] =<< translateTerm term

translate :: Bindings -> Program
translate globals =
  let actions = pushDefinition <$> Core.definitions globals
  in runTranslate (sequence actions) ^. the @"program"
