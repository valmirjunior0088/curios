module Curios.ParsingSpec (spec) where

import Test.Hspec (describe, it)

import Curios.Parsing
  ( name
  , identifier
  , atom
  , piBinding
  , lambdaBinding
  , abstraction
  , expression
  , statement
  , program
  )

import Curios.Expression
  ( Name
  , Identifier
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  )

spec =
  describe "Curios.ParsingSpec" $ do
    it "works" $
      True

