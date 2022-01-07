{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Curios.Core.Context
  ( Error
  , Declarations
  , Definitions
  , Context
  , Contextual
  , execContextual
  , insertDeclaration
  , insertDefinition
  )
  where

import Curios.Core.Error (Cause (..), Error (..))
import Control.Monad (unless, when)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, local)
import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import Control.Monad.Except (MonadError, Except, runExcept, throwError)

import Curios.Core.Term
  ( Origin
  , Name
  , Index
  , Type
  , Primitive (..)
  , Literal (..)
  , Operation (..)
  , Term (..)
  , getOrigin
  , shift
  , instantiate
  , trType
  , trPrimitive
  )

type Declarations = [(Name, Type)]
type Definitions = [(Name, Term)]

type Context = (Declarations, Definitions)

newtype Contextual a =
  ContextualT (StateT Context (Except Error) a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError Error
    )

execContextual :: Contextual () -> Either Error Context
execContextual (ContextualT action) =
  runExcept (execStateT action ([], []))

class MonadWhnf m where
  whnf :: Term -> m Term

instance MonadWhnf Contextual where
  whnf term =
    case term of
      TrReference origin name -> do
        (_, definitions) <- get

        case lookup name definitions of
          Nothing -> return (TrReference origin name)
          Just definition -> whnf definition

      TrApplication origin function argument -> do
        function' <- whnf function
        argument' <- whnf argument

        case function' of
          TrFunction _ output -> whnf (instantiate argument' output)
          _ -> return (TrApplication origin function' argument')
      
      TrCase origin scrutinee -> do
        scrutinee' <- whnf scrutinee

        case scrutinee' of
          TrData _ constructor -> whnf constructor
          _ -> return (TrCase origin scrutinee')

      _ ->
        return term

type Equation = (Term, Term)

newtype Converts a =
  Converts (ReaderT [Equation] Contextual a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError Error
    , MonadReader [Equation]
    )

runConverts :: Converts a -> Contextual a
runConverts (Converts action) =
  runReaderT action []

instance MonadWhnf Converts where
  whnf term =
    Converts (lift $ whnf term)

remembered :: Equation -> Converts Bool
remembered equation =
  elem equation <$> ask

rememberIn :: Equation -> Converts a -> Converts a
rememberIn equation action =
  local (equation :) action

(.&&.) :: Monad m => m Bool -> m Bool -> m Bool
(.&&.) one other = do
  result <- one

  case result of
    True -> other
    False -> return False

infixl .&&.

(.||.) :: Monad m => m Bool -> m Bool -> m Bool
(.||.) one other = do
  result <- one

  case result of
    True -> return True
    False -> other

infixl .||.

class MonadConverts m where
  converts :: Term -> Term -> m Bool

instance MonadConverts Converts where
  converts one other = do
    one' <- whnf one
    other' <- whnf other

    let
      equation =
        (one', other')

      isEqual =
        pure (one' == other')
      
      isRemembered =
        remembered equation
      
      isEquirecursive =
        case equation of
          (TrFunctionType _ input output, TrFunctionType _ input' output') ->
            converts input input' .&&. converts output output'
          
          (TrFunction _ output, TrFunction _ output') ->
            converts output output'
          
          (TrApplication _ function argument, TrApplication _ function' argument') ->
            converts function function' .&&. converts argument argument'
          
          (TrSelf _ output, TrSelf _ output') ->
            converts output output'

          (TrData _ constructor, TrData _ constructor') ->
            converts constructor constructor'
          
          (TrCase _ scrutinee, TrCase _ scrutinee') ->
            converts scrutinee scrutinee'
          
          (TrOperation _ operation, TrOperation _ operation') ->
            case (operation, operation') of
              (OpInt32Sum first second, OpInt32Sum first' second') ->
                converts first first' .&&. converts second second'
              
              (OpFlt32Sum first second, OpFlt32Sum first' second') ->
                converts first first' .&&. converts second second'
              
              _ ->
                return False

          _ ->
            return False
    
    isEqual .||. isRemembered .||. rememberIn equation isEquirecursive

newtype Check a =
  Check (ReaderT [Type] Contextual a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadReader [Type]
    , MonadError Error
    )

runCheck :: Check a -> Contextual a
runCheck (Check action) =
  runReaderT action []

instance MonadWhnf Check where
  whnf term =
    Check (lift $ whnf term)

instance MonadConverts Check where
  converts one other =
    Check (lift $ runConverts $ converts one other)

bindIn :: Type -> Check a -> Check a
bindIn typ action =
  local (map shift . (:) typ) action

at :: Index -> [a] -> Maybe a
at index values =
  case values of
    [] -> Nothing
    value : _ | index == 0 -> Just value
    _ : rest -> at (pred index) rest

bound :: Index -> Check (Maybe Type)
bound index =
  at index <$> ask

class MonadAbort m where
  abort :: Origin -> Cause -> m a

instance MonadAbort Check where
  abort origin cause =
    throwError (Error origin cause)

infer :: Term -> Check Type
infer term =
  case term of
    TrReference origin name -> do
      (declarations, _) <- get

      case lookup name declarations of
        Nothing -> abort origin (CsUndeclaredName name)
        Just declaration -> return declaration

    TrVariable origin index -> do
      result <- bound index

      case result of
        Nothing -> abort origin (CsVariableOutOfBounds index)
        Just variable -> return variable

    TrType _ ->
      return trType

    TrFunctionType _ input output -> do
      check trType input
      bindIn input (check trType output)
      return trType

    TrFunction origin _ ->
      abort origin CsFunctionsDontHaveAnInferableType

    TrApplication _ function argument -> do
      typ <- infer function >>= whnf

      case typ of
        TrFunctionType _ input output -> do
          check input argument
          return (instantiate argument output)

        _ ->
          abort (getOrigin function) CsFunctionDidntHaveFunctionType

    TrSelf origin output -> do
      bindIn (TrSelf origin output) (check trType output)
      return trType
    
    TrData origin _ ->
      abort origin CsConstructorsDontHaveAnInferableType
    
    TrCase _ scrutinee -> do
      typ <- infer scrutinee >>= whnf

      case typ of
        TrSelf _ output -> return (instantiate scrutinee output)
        _ -> abort (getOrigin scrutinee) CsConstructorDidntHaveSelfType
    
    TrPrimitive _ _ ->
      return trType
    
    TrLiteral _ literal ->
      case literal of
        LtInt32 _ -> return (trPrimitive PrInt32)
        LtFlt32 _ -> return (trPrimitive PrFlt32)
    
    TrOperation _ (OpInt32Sum one other) -> do
      check (trPrimitive PrInt32) one
      check (trPrimitive PrInt32) other
      return (trPrimitive PrInt32)
    
    TrOperation _ (OpFlt32Sum one other) -> do
      check (trPrimitive PrFlt32) one
      check (trPrimitive PrFlt32) other
      return (trPrimitive PrFlt32)

class MonadCheck m where
  check :: Type -> Term -> m ()

instance MonadCheck Check where
  check typ term = do
    typ' <- whnf typ
    
    case (typ', term) of
      (TrFunctionType _ input output, TrFunction _ output') ->
        bindIn input (check output output')
      
      (_, TrFunction origin _) ->
        abort origin CsFunctionTypeMismatch
      
      (TrSelf _ output, TrData origin constructor) ->
        check (instantiate (TrData origin constructor) output) constructor

      (_, TrData origin _) ->
        abort origin CsConstructorDidntHaveSelfType

      _ -> do
        typ'' <- infer term
        areConvertible <- converts typ' typ''
        unless areConvertible (abort (getOrigin term) CsTypeMismatch)

instance MonadAbort Contextual where
  abort origin cause =
    throwError (Error origin cause)

instance MonadCheck Contextual where
  check typ term =
    runCheck (check typ term)

insertDeclaration :: (Origin, Name) -> Type -> Contextual ()
insertDeclaration (origin, name) typ = do
  (declarations, definitions) <- get

  when (elem name $ map fst declarations)
    (abort origin $ CsNameAlreadyDeclared name)

  check trType typ

  put ((name, typ) : declarations, definitions)

insertDefinition :: (Origin, Name) -> Term -> Contextual ()
insertDefinition (origin, name) term = do
  (declarations, definitions) <- get
  
  when (elem name $ map fst definitions)
    (abort origin $ CsNameAlreadyDefined name)

  typ <- case lookup name declarations of
    Nothing -> abort origin (CsUndeclaredNameBeingDefined name)
    Just declaration -> return declaration
  
  put (declarations, (name, term) : definitions)
  
  check typ term
