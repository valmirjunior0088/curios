module Curios.TranslationSpec (spec) where

import Test.Hspec (describe, it)

import Curios.Expression
  ( Name
  , QualifiedName
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
  ( qnTranslate
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

