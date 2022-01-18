{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad (unless, when, zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State (MonadState (..), StateT, execStateT)
import Control.Monad.Except (MonadError (..), Except, runExcept)

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

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Declarations = Map Name Type
type Definitions = Map Name Term
type Context = (Declarations, Definitions)

newtype Contextual a =
  Contextual (StateT Context (Except Error) a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError Error
    )

execContextual :: Contextual () -> Either Error Context
execContextual (Contextual action) =
  runExcept (execStateT action (Map.empty, Map.empty))

class MonadWhnf m where
  whnf :: Term -> m Term

instance MonadWhnf Contextual where
  whnf term =
    case term of
      TrReference origin name -> do
        (_, definitions) <- get

        case Map.lookup name definitions of
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
  Converts (ReaderT (Seq Equation) Contextual a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError Error
    , MonadReader (Seq Equation)
    )

runConverts :: Converts a -> Contextual a
runConverts (Converts action) =
  runReaderT action Seq.empty

instance MonadWhnf Converts where
  whnf term =
    Converts (lift $ whnf term)

remembered :: Equation -> Converts Bool
remembered equation =
  elem equation <$> ask

remember :: Equation -> Converts a -> Converts a
remember equation action =
  local (Seq.|> equation) action

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
          
          (TrOperation _ operation arguments, TrOperation _ operation' arguments') ->
            pure (operation == operation') .&&. and <$> zipWithM converts arguments arguments'

          _ ->
            return False
    
    isEqual .||. isRemembered .||. remember equation isEquirecursive

newtype Check a =
  Check (ReaderT (Seq Type) Contextual a) deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadReader (Seq Type)
    , MonadError Error
    )

runCheck :: Check a -> Contextual a
runCheck (Check action) =
  runReaderT action Seq.empty

instance MonadWhnf Check where
  whnf term =
    Check (lift $ whnf term)

instance MonadConverts Check where
  converts one other =
    Check (lift $ runConverts $ converts one other)

bind :: Type -> Check a -> Check a
bind typ action =
  let go typs = fmap (shift 1) (typ Seq.<| typs) in local go action

bound :: Index -> Check (Maybe Type)
bound index =
  Seq.lookup index <$> ask

abort :: MonadError Error m => Origin -> Cause -> m a
abort origin cause =
  throwError (Error origin cause)

infer :: Term -> Check Type
infer term =
  case term of
    TrReference origin name -> do
      (declarations, _) <- get

      case Map.lookup name declarations of
        Nothing -> abort origin (CsUndeclaredName name)
        Just declaration -> return declaration

    TrVariable origin index -> do
      result <- bound index

      case result of
        Nothing -> abort origin (CsVariableOutOfBounds index)
        Just typ -> return typ

    TrType _ ->
      return trType

    TrFunctionType _ input output -> do
      check trType input
      bind input (check trType output)
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
      bind (TrSelf origin output) (check trType output)
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
    
    TrOperation _ OpInt32Sum [one, other] -> do
      check (trPrimitive PrInt32) one
      check (trPrimitive PrInt32) other
      return (trPrimitive PrInt32)
    
    TrOperation _ OpFlt32Sum [one, other] -> do
      check (trPrimitive PrFlt32) one
      check (trPrimitive PrFlt32) other
      return (trPrimitive PrFlt32)
    
    TrOperation origin operation _ ->
      abort origin (CsInvalidOperationFormat operation)

class MonadCheck m where
  check :: Type -> Term -> m ()

instance MonadCheck Check where
  check typ term = do
    typ' <- whnf typ
    
    case (typ', term) of
      (TrFunctionType _ input output, TrFunction _ output') ->
        bind input (check output output')
      
      (_, TrFunction origin _) ->
        abort origin CsFunctionTypeMismatch
      
      (TrSelf _ output, TrData origin constructor) ->
        check (instantiate (TrData origin constructor) output) constructor

      (_, TrData origin _) ->
        abort origin CsSelfTypeMismatch 

      _ -> do
        typ'' <- infer term
        areConvertible <- converts typ' typ''
        unless areConvertible (abort (getOrigin term) CsTypeMismatch)

instance MonadCheck Contextual where
  check typ term =
    runCheck (check typ term)

insertDeclaration :: (Origin, Name) -> Type -> Contextual ()
insertDeclaration (origin, name) typ = do
  (declarations, definitions) <- get

  when (Map.member name declarations)
    (abort origin $ CsNameAlreadyDeclared name)

  check trType typ

  put (Map.insert name typ declarations, definitions)

insertDefinition :: (Origin, Name) -> Term -> Contextual ()
insertDefinition (origin, name) term = do
  (declarations, definitions) <- get
  
  when (Map.member name definitions)
    (abort origin $ CsNameAlreadyDefined name)

  typ <- case Map.lookup name declarations of
    Nothing -> abort origin (CsUndeclaredNameBeingDefined name)
    Just typ -> return typ
  
  put (declarations, Map.insert name term definitions)
  
  check typ term
