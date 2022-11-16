import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util ((!!!))
import Error (Origin (..), showError)
import Core.Parse (parse)
import Core.Program (check)
import Core.Syntax (Primitive (..), Operation (..), Term (..), instantiate)
import Control.Monad.Reader (MonadReader (..), Reader, runReader, asks)
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

  PrimitiveType _ primitiveType -> return (PrimitiveType Machine primitiveType)
  Primitive _ primitive -> return (Primitive Machine primitive)

  Operate _ operation operands -> mapM (mapM evaluateTerm) (operation, operands) >>= \case
    (Int32Add, [Primitive _ (Int32 one), Primitive _ (Int32 other)]) ->
      return (Primitive Machine $ Int32 $ one + other)

    (Flt32Add, [Primitive _ (Flt32 one), Primitive _ (Flt32 other)]) ->
      return (Primitive Machine $ Flt32 $ one + other)

    _ -> return (Operate Machine operation operands)

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
