module Inter.Translate
  ( translate
  )
  where

import qualified Core.Syntax as Core
import qualified Core.Bindings as Core
import qualified Inter.Syntax as Inter

import Data.Int (Int32)
import Data.List (nub)
import Control.Monad (unless, forM)
import Control.Monad.State (MonadState, State, execState)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens ((^.), use, (<>=), (.=))

data TranslateState = TranslateState
  { program :: Inter.Program
  
  , nextLabel :: Int32
  , labels :: [(String, Int32)]

  , nextName :: Int
  , nextBlockName :: Int
  , nextClosureName :: Int
  , root :: String
  }
  deriving (Generic)

emptyState :: TranslateState
emptyState = TranslateState
  { program = mempty
  
  , nextLabel = 0
  , labels = []

  , nextName = 0
  , nextBlockName = 0
  , nextClosureName = 0
  , root = ""
  }

newtype Translate a = Translate (State TranslateState a)
  deriving (Functor, Applicative, Monad, MonadState TranslateState)

runTranslate :: Translate a -> Inter.Program
runTranslate (Translate action) = execState action emptyState ^. the @"program"

pushBlock :: String -> Inter.Scope Inter.Sequence -> Translate ()
pushBlock name body =
  (the @"program" . the @"blocks") <>= [(name, body)]

pushClosure :: String -> Inter.Scope (Inter.Scope Inter.Target) -> Translate ()
pushClosure name target =
  (the @"program" . the @"closures") <>= [(name, target)]

getLabel :: String -> Translate Int32
getLabel name = do
  labels <- use (the @"labels")

  case lookup name labels of
    Nothing -> do
      index <- use (the @"nextLabel")
      (the @"nextLabel") .= succ index

      (the @"labels") <>= [(name, index)]

      return index
    
    Just index -> return index

freshName :: Translate String
freshName = do
  index <- use (the @"nextName")
  (the @"nextName") .= succ index
  return (show index)

freshBlockName :: Translate String
freshBlockName = do
  root <- use (the @"root")
  index <- use (the @"nextBlockName")
  (the @"nextBlockName") .= succ index
  return (root ++ "_" ++ show index)

freshClosureName :: Translate String
freshClosureName = do
  root <- use (the @"root")
  index <- use (the @"nextClosureName")
  (the @"nextClosureName") .= succ index
  return (root ++ "_" ++ show index)

type Result = ([String], [(String, Inter.Expression)], Inter.Expression)

translateGlobal :: String -> Translate Result
translateGlobal variable =
  return ([], [], Inter.Jump Inter.Target { block = variable, atoms = [] })

translateLocal :: Core.Variable -> Translate Result
translateLocal variable =
  let name = Core.unwrap variable
  in return ([name], [], Inter.Pure $ Inter.free name)

translateNull :: Translate Result
translateNull =
  return ([], [], Inter.Pure Inter.Null)

translateFunction :: Core.Scope Core.Term -> Translate Result
translateFunction body = do
  name <- freshName
  (bodyFrees, bodyExpressions, bodyExpression) <- translateTerm (Core.open name body)

  blockName <- freshBlockName
  pushBlock blockName (Inter.abstract bodyFrees $ Inter.construct bodyExpressions bodyExpression)

  let
    environment = filter (/= name) bodyFrees

    abstractEnvironment = Inter.abstract environment
    abstractArgument = Inter.abstract [name]
    target = Inter.Target { block = blockName, atoms = map Inter.free bodyFrees }

  closureName <- freshClosureName
  pushClosure closureName (abstractEnvironment $ abstractArgument target)

  let
    frees = environment
    expressions = []
    expression = Inter.ClosureAlloc closureName (map Inter.free environment)

  return (nub frees, expressions, expression)

translateApply :: Core.Term -> Core.Term -> Translate Result
translateApply function argument = do
  functionName <- freshName
  (functionFrees, functionExpressions, functionExpression) <- translateTerm function
  
  argumentName <- freshName
  (argumentFrees, argumentExpressions, argumentExpression) <- translateTerm argument

  let
    frees = functionFrees ++ argumentFrees

    expressions = functionExpressions ++ [(functionName, functionExpression)]
      ++ argumentExpressions ++ [(argumentName, argumentExpression)]

    expression = Inter.ClosureEnter (Inter.free functionName) [Inter.free argumentName]

  return (nub frees, expressions, expression)

translatePair :: Core.Term -> Core.Term -> Translate Result
translatePair left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]
    
    expression = Inter.StructAlloc [Inter.free leftName, Inter.free rightName]

  return (nub frees, expressions, expression)

translateSplit :: Core.Term -> Core.Scope (Core.Scope Core.Term) -> Translate Result
translateSplit scrutinee body = do
  scrutineeName <- freshName
  (scrutineeFrees, scrutineeExpressions, scrutineeExpression) <- translateTerm scrutinee

  leftName <- freshName
  rightName <- freshName
  (bodyFrees, bodyExpressions, bodyExpression) <- translateTerm (Core.open rightName $ Core.open leftName body)

  let
    frees = scrutineeFrees ++ filter (`notElem` [leftName, rightName]) bodyFrees

    expressions = scrutineeExpressions
      ++ [(scrutineeName, scrutineeExpression)]
      ++ [(leftName, Inter.StructSelect (Inter.free scrutineeName) 0)]
      ++ [(rightName, Inter.StructSelect (Inter.free scrutineeName) 1)]
      ++ bodyExpressions

    expression = bodyExpression

  return (nub frees, expressions, expression)

translateLabel :: String -> Translate Result
translateLabel label = do
  index <- getLabel label
  return ([], [], Inter.Int32Alloc index)

translateMatch :: Core.Term -> [(String, Core.Term)] -> Translate Result
translateMatch scrutinee branches = do
  scrutineeName <- freshName
  (scrutineeFrees, scrutineeExpressions, scrutineeExpression) <- translateTerm scrutinee

  translatedBranches <- forM branches $ \(label, branch) -> do
    index <- getLabel label
    (branchFrees, branchExpressions, branchExpression) <- translateTerm branch

    blockName <- freshBlockName
    pushBlock blockName (Inter.abstract branchFrees $ Inter.construct branchExpressions branchExpression)

    return (branchFrees, (index, Inter.Target { block = blockName, atoms = map Inter.free branchFrees }))

  let
    (branchFrees, branchTargets) = unzip translatedBranches

    frees = scrutineeFrees ++ concat branchFrees
    expressions = scrutineeExpressions ++ [(scrutineeName, scrutineeExpression)]
    expression = Inter.Int32Match (Inter.free scrutineeName) branchTargets

  return (nub frees, expressions, expression)

translateInt32 :: Int32 -> Translate Result
translateInt32 value = return ([], [], Inter.Int32Alloc value)

translateInt32If :: Core.Term -> Core.Term -> Core.Term -> Translate Result
translateInt32If scrutinee truthy falsy = do
  scrutineeName <- freshName
  (scrutineeFrees, scrutineeExpressions, scrutineeExpression) <- translateTerm scrutinee

  truthyBlockName <- freshName
  (truthyFrees, truthyExpressions, truthyExpression) <- translateTerm truthy
  pushBlock truthyBlockName (Inter.abstract truthyFrees $ Inter.construct truthyExpressions truthyExpression)

  falsyBlockName <- freshName
  (falsyFrees, falsyExpressions, falsyExpression) <- translateTerm falsy
  pushBlock falsyBlockName (Inter.abstract falsyFrees $ Inter.construct falsyExpressions falsyExpression)

  let
    truthyTarget = Inter.Target { block = truthyBlockName, atoms = map Inter.free truthyFrees }
    falsyTarget = Inter.Target { block = falsyBlockName, atoms = map Inter.free falsyFrees }

    frees = scrutineeFrees ++ truthyFrees ++ falsyFrees
    expressions = scrutineeExpressions ++ [(scrutineeName, scrutineeExpression)]
    expression = Inter.Int32If (Inter.free scrutineeName) truthyTarget falsyTarget
  
  return (nub frees, expressions, expression)

translateInt32BinOp :: Core.BinOp -> Core.Term -> Core.Term -> Translate Result
translateInt32BinOp op left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]

    expression = Inter.Int32BinOp op (Inter.free leftName) (Inter.free rightName)
  
  return (nub frees, expressions, expression)

translateInt32BoolOp :: Core.BoolOp -> Core.Term -> Core.Term -> Translate Result
translateInt32BoolOp op left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]

    expression = Inter.Int32BoolOp op (Inter.free leftName) (Inter.free rightName)
  
  return (nub frees, expressions, expression)

translateInt32CompOp :: Core.CompOp -> Core.Term -> Core.Term -> Translate Result
translateInt32CompOp op left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]

    expression = Inter.Int32CompOp op (Inter.free leftName) (Inter.free rightName)
  
  return (nub frees, expressions, expression)

translateFlt32 :: Float -> Translate Result
translateFlt32 value = return ([], [], Inter.Flt32Alloc value)

translateFlt32BinOp :: Core.BinOp -> Core.Term -> Core.Term -> Translate Result
translateFlt32BinOp op left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]

    expression = Inter.Flt32BinOp op (Inter.free leftName) (Inter.free rightName)
  
  return (nub frees, expressions, expression)

translateFlt32CompOp :: Core.CompOp -> Core.Term -> Core.Term -> Translate Result
translateFlt32CompOp op left right = do
  leftName <- freshName
  (leftFrees, leftExpressions, leftExpression) <- translateTerm left

  rightName <- freshName
  (rightFrees, rightExpressions, rightExpression) <- translateTerm right

  let
    frees = leftFrees ++ rightFrees

    expressions = leftExpressions ++ [(leftName, leftExpression)]
      ++ rightExpressions ++ [(rightName, rightExpression)]

    expression = Inter.Flt32CompOp op (Inter.free leftName) (Inter.free rightName)
  
  return (nub frees, expressions, expression)

translateTerm :: Core.Term -> Translate Result
translateTerm = \case
  Core.Global _ variable -> translateGlobal variable
  Core.Local _ variable -> translateLocal variable
  Core.Type _ -> translateNull
  Core.FunctionType _ _ _ -> translateNull
  Core.Function _ body -> translateFunction body
  Core.Apply _ function argument -> translateApply function argument
  Core.PairType _ _ _ -> translateNull
  Core.Pair _ left right -> translatePair left right
  Core.Split _ scrutinee body -> translateSplit scrutinee body
  Core.LabelType _ _ -> translateNull
  Core.Label _ label -> translateLabel label
  Core.Match _ scrutinee branches -> translateMatch scrutinee branches
  Core.Int32Type _ -> translateNull
  Core.Int32 _ value -> translateInt32 value
  Core.Int32If _ scrutinee truthy falsy -> translateInt32If scrutinee truthy falsy
  Core.Int32BinOp _ op left right -> translateInt32BinOp op left right
  Core.Int32BoolOp _ op left right -> translateInt32BoolOp op left right
  Core.Int32CompOp _ op left right -> translateInt32CompOp op left right
  Core.Flt32Type _ -> translateNull
  Core.Flt32 _ value -> translateFlt32 value
  Core.Flt32BinOp _ op left right -> translateFlt32BinOp op left right
  Core.Flt32CompOp _ op left right -> translateFlt32CompOp op left right

pushDefinition :: (String, Core.Term) -> Translate ()
pushDefinition (name, term) = do
  (the @"nextName") .= 0
  (the @"nextBlockName") .= 0
  (the @"nextClosureName") .= 0
  (the @"root") .= name
  (frees, expressions, expression) <- translateTerm term
  unless (null frees) (error "free variables in top-level definition")
  pushBlock name (Inter.unbound $ Inter.construct expressions expression)

translate :: Core.Bindings -> Inter.Program
translate Core.Bindings { definitions } = runTranslate (mapM_ pushDefinition definitions)
