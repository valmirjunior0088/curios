module Curios.TranslationSpec (spec) where

import Test.Hspec (describe, it)

import Curios.Expression
  ( Name
  , Identifier
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  )

import Curios.Term
  ( Universe
  , Index
  , Type
  , Primitive (..)
  , Literal (..)
  , Term (..)
  )

import Curios.Translation
  ( idTranslate
  , atTranslate
  , teAbstract
  , teApplyPiBinding
  , teApplyLambdaBinding
  , abTranslate
  , exTranslate
  )

spec =
  describe "Curios.TranslationSpec" $ do
    it "works" $
      True

