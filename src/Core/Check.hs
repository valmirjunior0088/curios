module Core.Check
  ( check
  )
  where

import Core.Syntax
  ( Variable
  , wrap
  , unwrap
  , BoolOp (..)
  , BinOp (..)
  , CompOp (..)
  , Type
  , Term (..)
  , originates
  , instantiate
  , open
  )

import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings

import Util (unique, (<==>), (.&&.), both)
import Error (Origin (..), Error (..))
import Data.Functor ((<&>))
import Data.Bits (Bits (..))
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Except (MonadError (..), Except, runExcept)
import GHC.Generics (Generic)
import Data.Generics.Product (the)
import Control.Lens (view, (.~))

data CheckState = CheckState
  { globals :: Bindings
  , locals :: Bindings
  , seed :: Int
  }
  deriving (Show, Generic)

emptyState :: Bindings -> CheckState
emptyState globals = CheckState
  { globals = globals
  , locals = Bindings.empty
  , seed = 0
  }

newtype Check a =
  Check (ReaderT CheckState (Except Error) a)
  deriving (Functor, Applicative, Monad, MonadReader CheckState, MonadError Error)

runCheck :: Check a -> Bindings -> Either Error a
runCheck (Check action) globals = runExcept (runReaderT action $ emptyState globals)

declared :: String -> Check (Maybe Type)
declared name = Bindings.declaration name <$> view (the @"globals")

defined :: String -> Check (Maybe Term)
defined name = Bindings.definition name <$> view (the @"globals")

fresh :: (String -> Check b) -> Check b
fresh action = do
  seed <- view (the @"seed")

  let
    name = show seed
    updateSeed = (the @"seed") .~ succ seed

  local updateSeed (action name)

bind :: Type -> (String -> Check a) -> Check a
bind tipe action = do
  locals <- view (the @"locals")
  seed <- view (the @"seed")

  let
    name = show seed
    updateLocals = (the @"locals") .~ Bindings.declare name tipe locals
    updateSeed = (the @"seed") .~ succ seed
  
  local (updateLocals . updateSeed) (action name)

bound :: Variable -> Check (Maybe Type)
bound variable = Bindings.declaration (unwrap variable) <$> view (the @"locals")

constrain :: Variable -> Term -> Check a -> Check a
constrain variable term action = do
  locals <- view (the @"locals")

  let
    name = unwrap variable
    updateLocals = (the @"locals") .~ Bindings.define name term locals

  local updateLocals action

constrained :: Variable -> Check (Maybe Term)
constrained variable = Bindings.definition (unwrap variable) <$> view (the @"locals")

reduce :: Term -> Check Term
reduce = \case
  Global origin name ->
    defined name >>= \case
      Just term -> reduce term
      _ -> return (Global origin name)

  Local origin variable ->
    constrained variable >>= \case
      Just term -> reduce term
      _ -> return (Local origin variable)

  Apply origin function argument -> reduce function >>= \case
    Function _ body -> reduce (instantiate argument body)
    _ -> return (Apply origin function argument)

  Split origin scrutinee body -> reduce scrutinee >>= \case
    Pair _ left right -> reduce (instantiate right (instantiate left body))
    _ -> return (Split origin scrutinee body)

  Match origin scrutinee branches -> reduce scrutinee >>= \case
    Label _ label -> reduce (fromMaybe (error "unknown label") $ lookup label branches)
    _ -> return (Match origin scrutinee branches)

  Int32If origin scrutinee truthy falsy -> reduce scrutinee >>= \case
    Int32 _ value -> reduce (if value /= 0 then truthy else falsy)
    _ -> return (Int32If origin scrutinee truthy falsy)

  Int32BinOp origin op left right -> both (reduce left, reduce right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      Add -> Int32 Machine (left' + right')
      Sub -> Int32 Machine (left' - right')
      Mul -> Int32 Machine (left' * right')
      Div -> Int32 Machine (left' `quot` right')

    _ -> return (Int32BinOp origin op left right)

  Int32BoolOp origin op left right -> both (reduce left, reduce right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      And -> Int32 Machine (left' .&. right')
      Or -> Int32 Machine (left' .|. right')

    _ -> return (Int32BoolOp origin op left right)

  Int32CompOp origin op left right -> both (reduce left, reduce right) >>= \case
    (Int32 _ left', Int32 _ right') -> return $ case op of
      Eq -> Int32 Machine (if left' == right' then 1 else 0)
      Ne -> Int32 Machine (if left' /= right' then 1 else 0)
      Lt -> Int32 Machine (if left' < right' then 1 else 0)
      Le -> Int32 Machine (if left' <= right' then 1 else 0)
      Gt -> Int32 Machine (if left' > right' then 1 else 0)
      Ge -> Int32 Machine (if left' >= right' then 1 else 0)

    _ -> return (Int32CompOp origin op left right)

  Flt32BinOp origin op left right -> both (reduce left, reduce right) >>= \case
    (Flt32 _ left', Flt32 _ right') -> return $ case op of
      Add -> Flt32 Machine (left' + right')
      Sub -> Flt32 Machine (left' - right')
      Mul -> Flt32 Machine (left' * right')
      Div -> Flt32 Machine (left' / right')

    _ -> return (Flt32BinOp origin op left right)

  Flt32CompOp origin op left right -> both (reduce left, reduce right) >>= \case
    (Flt32 _ left', Flt32 _ right') -> return $ case op of
      Eq -> Int32 Machine (if left' == right' then 1 else 0)
      Ne -> Int32 Machine (if left' /= right' then 1 else 0)
      Lt -> Int32 Machine (if left' < right' then 1 else 0)
      Le -> Int32 Machine (if left' <= right' then 1 else 0)
      Gt -> Int32 Machine (if left' > right' then 1 else 0)
      Ge -> Int32 Machine (if left' >= right' then 1 else 0)

    _ -> return (Flt32CompOp origin op left right)

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
    (FunctionType _ input scope, FunctionType _ input' scope') ->
      fresh $ \name -> do
        let
          output = open name scope
          output' = open name scope'

        go input input' .&&. go output output'

    (Function _ body, Function _ body') ->
      fresh $ \name -> do
        let
          output = open name body
          output' = open name body'

        go output output'

    (Apply _ function argument, Apply _ function' argument') ->
      go function function' .&&. go argument argument'

    (PairType _ input scope, PairType _ input' scope') ->
      fresh $ \name -> do
        let
          output = open name scope
          output' = open name scope'

        go input input' .&&. go output output'

    (Pair _ left right, Pair _ left' right') ->
      go left left' .&&. go right right'

    (Split _ scrutinee body, Split _ scrutinee' body') ->
      fresh $ \left -> fresh $ \right -> do
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

    (Int32If _ scrutinee truthy falsy, Int32If _ scrutinee' truthy' falsy') ->
      go scrutinee scrutinee' .&&. go truthy truthy' .&&. go falsy falsy'

    (Int32BinOp _ op left right, Int32BinOp _ op' left' right') ->
      pure (op == op') .&&. go left left' .&&. go right right'

    (Int32BoolOp _ op left right, Int32BoolOp _ op' left' right') ->
      pure (op == op') .&&. go left left' .&&. go right right'

    (Int32CompOp _ op left right, Int32CompOp _ op' left' right') ->
      pure (op == op') .&&. go left left' .&&. go right right'

    (Flt32BinOp _ op left right, Flt32BinOp _ op' left' right') ->
      pure (op == op') .&&. go left left' .&&. go right right'

    (Flt32CompOp _ op left right, Flt32CompOp _ op' left' right') ->
      pure (op == op') .&&. go left left' .&&. go right right'

    _ -> return False

equal :: Term -> Term -> Check Bool
equal = equals []

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

ifExpressionsDontHaveAnInferableType :: Origin -> Check a
ifExpressionsDontHaveAnInferableType origin = throwError Error
  { origin = origin
  , message = "Check error:\nIf expressions don't have an inferable type"
  }

infers :: Term -> Check Type
infers = \case
  Global origin name ->
    declared name >>= \case
      Nothing -> unknownGlobal origin name
      Just tipe -> return tipe

  Local _ variable -> do
    bound variable >>= \case
      Nothing -> error "unknown local variable -- should not happen"
      Just tipe -> return tipe

  Type _ -> return (Type Machine)

  FunctionType _ input scope -> do
    checks (Type Machine) input

    bind input $ \name -> do
      checks (Type Machine) (open name scope)
      return (Type Machine)

  Function origin _ -> functionsDontHaveAnInferableType origin

  Apply origin function argument -> infers function >>= reduce >>= \case
    FunctionType _ input scope -> do
      checks input argument
      return (instantiate argument scope)

    _ -> applicationTypeMismatch origin

  PairType _ input scope -> do
    checks (Type Machine) input

    bind input $ \name -> do
      checks (Type Machine) (open name scope)
      return (Type Machine)

  Pair origin _ _ -> pairsDontHaveAnInferableType origin

  Split origin _ _ -> splitExpressionsDontHaveAnInferableType origin

  LabelType origin set -> do
    unless (unique set) (labelTypeHasRepeatedLabels origin)
    return (Type Machine)

  Label origin _ -> labelsDontHaveAnInferableType origin

  Match origin _ _ -> matchExpressionsDontHaveAnInferableType origin

  Int32Type _ -> return (Type Machine)

  Int32 _ _ -> return (Int32Type Machine)

  Int32If origin _ _ _ -> ifExpressionsDontHaveAnInferableType origin

  Int32BinOp _ _ left right -> do
    checks (Int32Type Machine) left
    checks (Int32Type Machine) right
    return (Int32Type Machine)

  Int32BoolOp _ _ left right -> do
    checks (Int32Type Machine) left
    checks (Int32Type Machine) right
    return (Int32Type Machine)

  Int32CompOp _ _ left right -> do
    checks (Int32Type Machine) left
    checks (Int32Type Machine) right
    return (Int32Type Machine)

  Flt32Type _ -> return (Type Machine)

  Flt32 _ _ -> return (Flt32Type Machine)

  Flt32BinOp _ _ left right -> do
    checks (Flt32Type Machine) left
    checks (Flt32Type Machine) right
    return (Flt32Type Machine)

  Flt32CompOp _ _ left right -> do
    checks (Flt32Type Machine) left
    checks (Flt32Type Machine) right
    return (Int32Type Machine)

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
    FunctionType _ input scope ->
      bind input $ \name -> checks (open name scope) (open name body)

    _ -> functionTypeMismatch origin

  Pair origin left right -> reduce tipe >>= \case
    PairType _ input scope -> do
      checks input left
      checks (instantiate left scope) right

    _ -> pairTypeMismatch origin

  Split origin scrutinee body -> infers scrutinee >>= reduce >>= \case
    PairType _ input scope ->
      bind input $ \left -> bind (open left scope) $ \right ->
        reduce scrutinee >>= \case
          Local _ variable ->
            constrain variable pair (checks tipe (open right (open left body))) where
              leftComponent = Local Machine (wrap left)
              rightComponent = Local Machine (wrap right)
              pair = Pair Machine leftComponent rightComponent
            
          _ -> checks tipe (open right (open left body))

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
          Local _ variable -> \(label, body) ->
            constrain variable (Label Machine label) (checks tipe body)

          _ -> \(_, body) -> checks tipe body

        mapM_ go branches

      _ -> matchExpressionScrutineeTypeMismatch (originates scrutinee)

  Int32If _ scrutinee truthy falsy -> do
    checks (Int32Type Machine) scrutinee
    checks tipe truthy
    checks tipe falsy

  term -> do
    tipe' <- infers term
    areEqual <- equal tipe tipe'
    unless areEqual (typeMismatch $ originates term)

check :: Bindings -> Type -> Term -> Either Error ()
check globals tipe term = runCheck (checks tipe term) globals
