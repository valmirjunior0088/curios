module Curios.Context
  (Context (..)
  ,coEmpty
  ,coInsert
  ,coLookup
  )
  where

import Prelude hiding
  (lookup
  )

import Curios.Expression
  (Name (..)
  )

import Curios.Term
  (Term (..)
  )

import Data.Map.Strict
  (Map
  ,empty
  ,insert
  ,lookup
  )

newtype Context =
  Context (Map Name Term)

coEmpty :: Context
coEmpty =
  Context empty

coInsert :: Name -> Term -> Context -> Either String Context
coInsert name term (Context context) =
  case lookup name context of
    Nothing -> Right (Context (insert name term context))
    Just _ -> Left ("Free variable `" ++ show name ++ "` already exists in the target context")

coLookup :: Name -> Context -> Either String Term
coLookup name (Context context) =
  case lookup name context of
    Nothing -> Left ("Free variable `" ++ show name ++ "` does not exist in the target context")
    Just term -> Right term