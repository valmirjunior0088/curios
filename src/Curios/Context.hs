{-# LANGUAGE DeriveFunctor #-}

module Curios.Context
  (Context
  ,cnEmpty
  ,cnInsert
  ,cnLookup
  )
  where

import Curios.Expression
  (Identifier
  )

import Curios.Term
  (Name (..)
  ,Term (..)
  ,trShift
  )

newtype Context a =
  Context [(Identifier, a)]
  deriving (Functor)

cnEmpty :: Context Term
cnEmpty =
  Context []

cnInsert :: Identifier -> Term -> Context Term -> Context Term
cnInsert identifier term (Context context) =
  fmap (trShift 1 identifier) (Context ((identifier, term) : context))

cnLookup :: Name -> Context Term -> Maybe Term
cnLookup name@(Name identifier index) (Context context) =
  case context of
    [] -> Nothing

    ((identifier', term) : context')
      | identifier /= identifier' -> cnLookup name (Context context')
      | index > 0 -> cnLookup (Name identifier (pred index)) (Context context')
      | index == 0 -> Just term
      | otherwise -> Nothing
