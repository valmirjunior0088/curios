import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util ((!!!), both)
import Error (Origin (..), showError)
import Core.Parse (parse)
import Core.Program (check)
import Core.Syntax (BinOp (..), BoolOp (..), CompOp (..), Term (..), instantiate)
import Control.Monad.Reader (MonadReader (..), Reader, runReader, asks)
import Data.Bits (Bits (..))
import System.Exit (die)
import System.Environment (getArgs, getProgName)

newtype Evaluate a =
  Evaluate (Reader Bindings a)
  deriving (Functor, Applicative, Monad, MonadReader Bindings)

runEvaluate :: Evaluate a -> Bindings -> a
runEvaluate (Evaluate action) = runReader action

evaluateTerm :: Term -> Evaluate Term
evaluateTerm = \case
  Global _ name -> asks (Bindings.definition name) >>= \case
    Nothing -> error "evaluation: unknown global"
    Just term -> evaluateTerm term

  Local _ variable -> return (Local Machine variable)
  Type _ -> return (Type Machine)
  FunctionType _ input body -> FunctionType Machine <$> evaluateTerm input <*> pure body
  Function _ body -> return (Function Machine body)

  Apply _ function argument -> evaluateTerm function >>= \case
    Function _ body -> evaluateTerm (instantiate argument body)
    _ -> Apply Machine <$> evaluateTerm function <*> evaluateTerm argument

  PairType _ input body -> PairType Machine <$> evaluateTerm input <*> pure body
  Pair _ left right -> Pair Machine <$> evaluateTerm left <*> evaluateTerm right

  Split _ scrutinee body -> evaluateTerm scrutinee >>= \case
    Pair _ left right -> evaluateTerm (instantiate right (instantiate left body))
    _ -> Split Machine <$> evaluateTerm scrutinee <*> pure body

  LabelType _ labels -> return (LabelType Machine labels)
  Label _ label -> return (Label Machine label)

  Match _ scrutinee branches -> evaluateTerm scrutinee >>= \case
    Label _ label -> evaluateTerm (label !!! branches)
    _ -> Match Machine <$> evaluateTerm scrutinee <*> pure branches

  Int32Type _ -> return (Int32Type Machine)
  Int32 _ value -> return (Int32 Machine value)

  Int32If _ scrutinee truthy falsy -> evaluateTerm scrutinee >>= \case
    Int32 _ value -> evaluateTerm (if value /= 0 then truthy else falsy)
    _ -> Int32If Machine <$> evaluateTerm scrutinee <*> evaluateTerm truthy <*> evaluateTerm falsy

  Int32BinOp _ op left right -> both (evaluateTerm left, evaluateTerm right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      Add -> Int32 Machine (left' + right')
      Sub -> Int32 Machine (left' - right')
      Mul -> Int32 Machine (left' * right')
      Div -> Int32 Machine (left' `quot` right')

    _ -> Int32BinOp Machine op <$> evaluateTerm left <*> evaluateTerm right

  Int32BoolOp _ op left right -> both (evaluateTerm left, evaluateTerm right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      And -> Int32 Machine (left' .&. right')
      Or -> Int32 Machine (left' .|. right')

    _ -> Int32BoolOp Machine op <$> evaluateTerm left <*> evaluateTerm right

  Int32CompOp _ op left right -> both (evaluateTerm left, evaluateTerm right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      Eq -> Int32 Machine (if left' == right' then 1 else 0)
      Ne -> Int32 Machine (if left' /= right' then 1 else 0)
      Lt -> Int32 Machine (if left' < right' then 1 else 0)
      Le -> Int32 Machine (if left' <= right' then 1 else 0)
      Gt -> Int32 Machine (if left' > right' then 1 else 0)
      Ge -> Int32 Machine (if left' >= right' then 1 else 0)

    _ -> Int32CompOp Machine op <$> evaluateTerm left <*> evaluateTerm right

  Flt32Type _ -> return (Flt32Type Machine)
  Flt32 _ value -> return (Flt32 Machine value)

  Flt32BinOp _ op left right -> both (evaluateTerm left, evaluateTerm right) >>= \case
    (Flt32 _ left', Flt32 _ right') -> return $ case op of
      Add -> Flt32 Machine (left' + right')
      Sub -> Flt32 Machine (left' - right')
      Mul -> Flt32 Machine (left' * right')
      Div -> Flt32 Machine (left' / right')

    _ -> Flt32BinOp Machine op <$> evaluateTerm left <*> evaluateTerm right

  Flt32CompOp _ op left right -> both (evaluateTerm left, evaluateTerm right) >>= \case
    (Flt32 _ left', Flt32 _ right') -> return $ case op of
      Eq -> Int32 Machine (if left' == right' then 1 else 0)
      Ne -> Int32 Machine (if left' /= right' then 1 else 0)
      Lt -> Int32 Machine (if left' < right' then 1 else 0)
      Le -> Int32 Machine (if left' <= right' then 1 else 0)
      Gt -> Int32 Machine (if left' > right' then 1 else 0)
      Ge -> Int32 Machine (if left' >= right' then 1 else 0)

    _ -> Flt32CompOp Machine op <$> evaluateTerm left <*> evaluateTerm right

evaluate :: Bindings -> Term -> Term
evaluate bindings term = runEvaluate (evaluateTerm term) bindings

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT NAME"

main :: IO ()
main = do
  (input, name) <- getArgs >>= \case
    [input, name] -> return (input, name)
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case parse source of
    Left reason -> die (showError source reason)
    Right program -> return program

  bindings <- case check program of
    Left reason -> die (showError source reason)
    Right bindings -> return bindings

  case evaluate bindings <$> Bindings.definition name bindings of
    Nothing -> putStrLn ("Could not evaluate \"" ++ name ++ "\" because it does not exist")
    Just result -> putStrLn ("Evaluation result: " ++ show result)
