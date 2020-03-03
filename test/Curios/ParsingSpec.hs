module Curios.ParsingSpec (spec) where

import qualified Text.Megaparsec as Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Test.Hspec (describe, it)

import Curios.Parsing
  ( name
  , qualifiedName
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
  , QualifiedName
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  )

spec =
  describe "Curios.ParsingSpec" $ do
    describe "name" $ do
      let parse = Megaparsec.parse name ""

      it "succeeds on a valid name" $
        parse "name" `shouldParse` "name"
      
      it "fails on an invalid name" $
        parse `shouldFailOn` "@invalid"
    
    describe "qualifiedName" $ do
      let parse = Megaparsec.parse qualifiedName ""

      it "succeeds on a valid qualified name" $
        parse "first;second" `shouldParse` ["first", "second"]
      
      it "fails on an invalid qualified name (1)" $
        parse `shouldFailOn` "first; second"
      
      it "fails on an invalid qualified name (2)" $
        parse `shouldFailOn` ";second"

    describe "atom" $ do
      let parse = Megaparsec.parse atom ""

      it "succeeds on a character" $
        parse "'a" `shouldParse` AtCharacter 'a'
      
      it "succeeds on a string" $
        parse "\"string\"" `shouldParse` AtString "string"
      
      it "succeeds on a positive integer" $
        parse "15" `shouldParse` AtInteger 15

      it "succeeds on a negative rational" $
        parse "-15.0" `shouldParse` AtRational (-15.0)
      
      it "succeeds on a symbol with a dash" $
        parse "some-name-with;dashes" `shouldParse` AtSymbol ["some-name-with", "dashes"]
