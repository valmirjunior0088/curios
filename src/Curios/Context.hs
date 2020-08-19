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

newtype Context =
  Context [(Identifier, Term)]

cnEmpty :: Context
cnEmpty =
  Context []

cnInsert :: Identifier -> Term -> Context -> Context
cnInsert identifier term (Context context) =
  let cnShift (identifier', term') = (identifier', trShift 1 identifier term') in
    Context (fmap cnShift ((identifier, term) : context))

cnLookup :: Name -> Context -> Maybe Term
cnLookup name@(Name identifier index) (Context context) =
  case context of
    [] -> Nothing

    ((identifier', term) : context')
      | identifier /= identifier' -> cnLookup name (Context context')
      | index > 0 -> cnLookup (Name identifier (pred index)) (Context context')
      | index == 0 -> Just term
      | otherwise -> Nothing
