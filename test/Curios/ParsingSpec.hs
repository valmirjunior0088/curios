module Curios.ParsingSpec (spec) where

import Text.Megaparsec (parse)
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
      it "succesfully parses a valid name" $
        parse name "" "name " `shouldParse` "name"
      
      it "erroneously parses an invalid name" $
        parse name "" `shouldFailOn` "@invalid "
    
    describe "qualifiedName" $ do
      it "succesfully parses a valid qualified name" $
        parse qualifiedName "" "first;second " `shouldParse` ["first", "second"]
      
      it "erroneously parses an invalid qualified name" $
        parse qualifiedName "" `shouldFailOn` "first; second "

    describe "atom" $ do
      it "successfully parses a negative rational" $
        parse atom "" "-15.0 " `shouldParse` AtRational (-15.0)
      
      it "successfully parses a symbol with a dash" $
        parse atom "" "some-name-with;dashes " `shouldParse` AtSymbol ["some-name-with", "dashes"]