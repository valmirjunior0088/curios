{-# LANGUAGE OverloadedStrings #-}

module Curios.ParsingSpec (spec) where

import qualified Text.Megaparsec as Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Test.Hspec (describe, it)

import Curios.Parsing
  (literal
  ,identifier
  ,binding
  ,bindings
  ,expression
  ,statement
  )

import Curios.Expression
  (Literal (..)
  ,Identifier
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )

spec =
  describe "Curios.ParsingSpec" $ do
    describe "name" $ do
      let parse = Megaparsec.parse identifier ""

      it "succeeds on a valid identifier" $
        parse "identifier" `shouldParse` "identifier"
      
      it "fails on an invalid identifier" $
        parse `shouldFailOn` "@invalid"

    describe "literal" $ do
      let parse = Megaparsec.parse literal ""

      it "succeeds on a character" $
        parse "'a'" `shouldParse` LtCharacter 'a'
      
      it "succeeds on text" $
        parse "\"text\"" `shouldParse` LtText "text"
      
      it "succeeds on a positive integer" $
        parse "15" `shouldParse` LtInteger 15

      it "succeeds on a negative rational" $
        parse "-15.0" `shouldParse` LtRational (-15.0)
    
    describe "binding" $ do
      let parse = Megaparsec.parse binding ""

      it "succeeds on a valid binding" $
        parse "identifier: type" `shouldParse`
          Binding "identifier" (ExIdentifier "type")
      
      it "fails on an invalid binding" $
        parse `shouldFailOn` "name:"
    
    describe "bindings" $ do
      let parse = Megaparsec.parse bindings ""

      it "succeeds on a valid binding sequence" $
        parse "identifier: type. identifier: type." `shouldParse`
          [Binding "identifier" (ExIdentifier "type")
          ,Binding "identifier" (ExIdentifier "type")
          ]
    
    describe "expression" $ do
      let parse = Megaparsec.parse expression ""
      
      it "succeeds on a pi abstraction" $
        parse "[identifier: type. identifier: type. type]" `shouldParse`
          ExAbstractionType
            [Binding "identifier" (ExIdentifier "type")
            ,Binding "identifier" (ExIdentifier "type")
            ]
            (ExIdentifier "type")
      
      it "fails on an invalid pi abstraction" $
        parse `shouldFailOn` "[type. identifier: type. type.]"
      
      it "succeeds on a lambda abstraction" $
        parse "{identifier: type. identifier: type. identifier}" `shouldParse`
          ExAbstraction
            [Binding "identifier" (ExIdentifier "type")
            ,Binding "identifier" (ExIdentifier "type")
            ]
            (ExIdentifier "identifier")
      
      it "fails on an invalid lambda abstraction" $
        parse `shouldFailOn` "{identifier. identifier: type. identifier.}"

      it "succeeds on an application" $
        parse "(function first-argument second-argument)" `shouldParse`
          ExApplication
            (ExIdentifier "function")
            [ExIdentifier "first-argument"
            ,ExIdentifier "second-argument"
            ]

    describe "statement" $ do
      let parse = Megaparsec.parse statement ""
      
      it "succeeds on a definition" $
        parse "def identity {a: type. value: a. value} end" `shouldParse`
          StDef "identity"
            (ExAbstraction
              [Binding "a" (ExIdentifier "type")
              ,Binding "value" (ExIdentifier "a")
              ]
              (ExIdentifier "value")
            )
