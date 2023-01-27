module Core.Check
  ( check
  )
  where

import Core.Syntax
  ( Variable (..)
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Type
  , Term (..)
  , originates
  , instantiate
  , open
  )

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util ((!!!), unique, (<==>), (.&&.))
import Error (Origin (..), Error (..))
import Data.Functor ((<&>))
import Control.Monad (unless, zipWithM)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.Except (MonadError (..), Except, runExcept)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (use, (.=), (%=))

data CheckState = CheckState
  { seed :: Integer
  , globals :: Bindings
  , locals :: Bindings
  }
  deriving (Show, Generic)

emptyState :: Bindings -> CheckState
emptyState globals = CheckState
  { seed = 0
  , globals = globals
  , locals = Bindings.empty
  }

newtype Check a =
  Check (StateT CheckState (Except Error) a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError Error)

runCheck :: Check a -> Bindings -> Either Error a
runCheck (Check action) globals = runExcept (evalStateT action $ emptyState globals)

region :: Check a -> Check a
region action = do
  locals <- use (the @"locals")
  result <- action
  (the @"locals") .= locals
  return result

fresh :: Check String
fresh = do
  seed <- use (the @"seed")
  (the @"seed") .= succ seed
  return (show seed)

reduce :: Term -> Check Term
reduce = \case
  Variable origin (Global name) -> do
    definition <- Bindings.definition name <$> use (the @"globals")

    case definition of
      Just term -> reduce term
      _ -> return (Variable origin $ Global name)

  Variable origin (LocalFree name) -> do
    definition <- Bindings.definition name <$> use (the @"locals")

    case definition of
      Just term -> reduce term
      _ -> return (Variable origin $ LocalFree name)

  Variable _ _ -> error "bound variable -- should not happen"

  Apply origin function argument -> reduce function >>= \case
    Function _ body -> reduce (instantiate argument body)
    _ -> return (Apply origin function argument)

  Split origin scrutinee body -> reduce scrutinee >>= \case
    Pair _ left right -> reduce (instantiate right (instantiate left body))
    _ -> return (Split origin scrutinee body)

  Match origin scrutinee branches -> reduce scrutinee >>= \case
    Label _ label -> reduce (label !!! branches)
    _ -> return (Match origin scrutinee branches)

  Operate origin operation operands -> mapM (mapM reduce) (operation, operands) >>= \case
    (Int32Add, [Primitive _ (Int32 one), Primitive _ (Int32 other)]) ->
      return (Primitive origin $ Int32 $ one + other)

    (Flt32Add, [Primitive _ (Flt32 one), Primitive _ (Flt32 other)]) ->
      return (Primitive origin $ Flt32 $ one + other)

    _ -> return (Operate origin operation operands)

  term -> return term

equals :: [(Term, Term)] -> Term -> Term -> Check Bool
equals history one other = do
  one' <- reduce one
  other' <- reduce other

  let
    isEqual = one' == other'
    isRemembered = (one', other') `elem` history
    go = equals ((one', other') : history)

  if isEqual || isRemembered then return True else case (one', other') of
    (FunctionType _ input scope, FunctionType _ input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'

    (Function _ body, Function _ body') -> do
      name <- fresh

      let
        output = open name body
        output' = open name body'

      go output output'

    (Apply _ function argument, Apply _ function' argument') ->
      go function function' .&&. go argument argument'

    (PairType _ input scope, PairType _ input' scope') -> do
      name <- fresh

      let
        output = open name scope
        output' = open name scope'

      go input input' .&&. go output output'

    (Pair _ left right, Pair _ left' right') ->
      go left left' .&&. go right right'

    (Split _ scrutinee body, Split _ scrutinee' body') -> do
      left <- fresh
      right <- fresh

      let
        output = open right (open left body)
        output' = open right (open left body')

      go scrutinee scrutinee' .&&. go output output'

    (Match _ scrutinee branches, Match _ scrutinee' branches') -> do
      let
        labelsAreEqual = map fst branches <==> map fst branches'

        contains (label, target) targets = case lookup label targets of
          Nothing -> return False
          Just body -> go body target

        bodiesAreEqual = and <$> mapM (`contains` branches) branches'

      go scrutinee scrutinee' .&&. pure labelsAreEqual .&&. bodiesAreEqual

    (Operate _ operation parameters, Operate _ operation' parameters') -> do
      let
        operationsAreEqual = operation == operation'
        parametersAreEqual = and <$> zipWithM go parameters parameters'

      pure operationsAreEqual .&&. parametersAreEqual

    (_, _) -> return False

equal :: Term -> Term -> Check Bool
equal = equals []

bind :: Type -> Check String
bind tipe = do
  name <- fresh
  (the @"locals") %= Bindings.declare name tipe
  return name

constrain :: String -> Term -> Check ()
constrain name term = do
  (the @"locals") %= Bindings.define name term

unknownGlobal :: Origin -> String -> Check a
unknownGlobal origin name = throwError Error
  { origin = origin
  , message = "Check error:\nUnknown global `" ++ name ++ "`"
  }

functionsDontHaveAnInferableType :: Origin -> Check a
functionsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nFunctions don't have an inferable type"
  }

applicationTypeMismatch :: Origin -> Check a
applicationTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nApplication type mismatch"
  }

pairsDontHaveAnInferableType :: Origin -> Check a
pairsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nPairs don't have an inferable type"
  }

splitExpressionsDontHaveAnInferableType :: Origin -> Check a
splitExpressionsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nSplit expressions don't have an inferable type"
  }

labelTypeHasRepeatedLabels :: Origin -> Check a
labelTypeHasRepeatedLabels origin = throwError Error
  { message = "Check error:\nLabel type has repeated labels"
  , origin = origin
  }

labelsDontHaveAnInferableType :: Origin -> Check a
labelsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nLabels don't have an inferable type"
  }

matchExpressionsDontHaveAnInferableType :: Origin -> Check a
matchExpressionsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nMatch expressions don't have an inferable type"
  }

invalidOperationFormat :: Origin -> Check a
invalidOperationFormat origin = throwError Error
  { origin = origin
  , message = "Check error:\nInvalid operation format"
  }

infers :: Term -> Check Type
infers = \case
  Variable origin (Global name) -> do
    declaration <- Bindings.declaration name <$> use (the @"globals")

    case declaration of
      Nothing -> unknownGlobal origin name
      Just tipe -> return tipe

  Variable _ (LocalFree variable) -> do
    declaration <- Bindings.declaration variable <$> use (the @"locals")

    case declaration of
      Nothing -> error "unknown local variable -- should not happen"
      Just tipe -> return tipe

  Variable _ _ -> error "bound variable -- should not happen"

  Type _ -> return (Type Machine)

  FunctionType _ input scope -> region $ do
    checks (Type Machine) input

    name <- bind input
    checks (Type Machine) (open name scope)

    return (Type Machine)

  Function origin _ -> functionsDontHaveAnInferableType origin

  Apply origin function argument -> infers function >>= reduce >>= \case
    FunctionType _ input scope -> do
      checks input argument
      return (instantiate argument scope)

    _ -> applicationTypeMismatch origin

  PairType _ input scope -> region $ do
    checks (Type Machine) input

    name <- bind input
    checks (Type Machine) (open name scope)

    return (Type Machine)

  Pair origin _ _ -> pairsDontHaveAnInferableType origin

  Split origin _ _ -> splitExpressionsDontHaveAnInferableType origin

  LabelType origin set -> do
    unless (unique set) (labelTypeHasRepeatedLabels origin)
    return (Type Machine)

  Label origin _ -> labelsDontHaveAnInferableType origin

  Match origin _ _ -> matchExpressionsDontHaveAnInferableType origin

  PrimitiveType _ _ -> return (Type Machine)

  Primitive _ primitive -> case primitive of
    Int32 _ -> return (PrimitiveType Machine Int32Type)
    Flt32 _ -> return (PrimitiveType Machine Flt32Type)

  Operate _ Int32Add [one, other] -> do
    checks (PrimitiveType Machine Int32Type) one
    checks (PrimitiveType Machine Int32Type) other
    return (PrimitiveType Machine Int32Type)

  Operate _ Flt32Add [one, other] -> do
    checks (PrimitiveType Machine Flt32Type) one
    checks (PrimitiveType Machine Flt32Type) other
    return (PrimitiveType Machine Flt32Type)

  Operate origin _ _ -> invalidOperationFormat origin

functionTypeMismatch :: Origin -> Check a
functionTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nFunction type mismatch"
  }

pairTypeMismatch :: Origin -> Check a
pairTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nPair type mismatch"
  }

splitExpressionScrutineeTypeMismatch :: Origin -> Check a
splitExpressionScrutineeTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nSplit expression scrutinee type mismatch"
  }

labelDoesNotBelongToLabelType :: Origin -> Check a
labelDoesNotBelongToLabelType origin = throwError Error
  { origin = origin
  , message = "Check error:\nLabel does not belong to label type"
  }

labelTypeMismatch :: Origin -> Check a
labelTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nLabel type mismatch"
  }

matchExpressionHasRepeatedBranchLabels :: Origin -> Check a
matchExpressionHasRepeatedBranchLabels origin = throwError Error
  { origin = origin
  , message = "Check error:\nMatch expression has repeated branch labels"
  }

matchExpressionBranchLabelsMismatch :: Origin -> Check a
matchExpressionBranchLabelsMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nMatch expression branch labels mismatch"
  }

matchExpressionScrutineeTypeMismatch :: Origin -> Check a
matchExpressionScrutineeTypeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nMatch expression scrutinee type mismatch"
  }

typeMismatch :: Origin -> Check a
typeMismatch origin = throwError Error
  { origin = origin
  , message = "Check error:\nType mismatch"
  }

checks :: Type -> Term -> Check ()
checks tipe = \case
  Function origin body -> reduce tipe >>= \case
    FunctionType _ input scope -> region $ do
      name <- bind input
      checks (open name scope) (open name body)

    _ -> functionTypeMismatch origin

  Pair origin left right -> reduce tipe >>= \case
    PairType _ input scope -> do
      checks input left
      checks (instantiate left scope) right

    _ -> pairTypeMismatch origin

  Split origin scrutinee body -> infers scrutinee >>= reduce >>= \case
    PairType _ input scope -> region $ do
      left <- bind input
      right <- bind (open left scope)

      reduce scrutinee >>= \case
        Variable _ (LocalFree variable) -> constrain variable pair where
          leftComponent = Variable Machine (LocalFree left)
          rightComponent = Variable Machine (LocalFree right)
          pair = Pair Machine leftComponent rightComponent

        _ -> return ()

      checks tipe (open right (open left body))

    _ -> splitExpressionScrutineeTypeMismatch origin

  Label origin label -> reduce tipe >>= \case
    LabelType _ labels ->
      unless (label `elem` labels) (labelDoesNotBelongToLabelType origin)

    _ -> labelTypeMismatch origin

  Match origin scrutinee branches -> do
    let labels = map fst branches

    unless (unique labels)
      (matchExpressionHasRepeatedBranchLabels origin)

    infers scrutinee >>= reduce >>= \case
      LabelType _ set -> do
        unless (labels <==> set)
          (matchExpressionBranchLabelsMismatch origin)

        go <- reduce scrutinee <&> \case
          Variable _ (LocalFree variable) -> \(label, body) -> region $ do
            constrain variable (Label Machine label)
            checks tipe body

          _ -> \(_, body) -> checks tipe body

        mapM_ go branches

      _ -> matchExpressionScrutineeTypeMismatch (originates scrutinee)

  term -> do
    tipe' <- infers term
    areEqual <- equal tipe tipe'
    unless areEqual (typeMismatch $ originates term)

check :: Bindings -> Type -> Term -> Either Error ()
check globals tipe term = runCheck (checks tipe term) globals
