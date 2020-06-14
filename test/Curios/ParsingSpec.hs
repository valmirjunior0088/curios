{-# LANGUAGE OverloadedStrings #-}

module Curios.ParsingSpec (spec) where

import qualified Text.Megaparsec as Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Test.Hspec (describe, it)

import Curios.Parsing
  (literal
  ,name
  ,binding
  ,bindings
  ,expression
  ,statement
  )

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )

spec =
  describe "Curios.ParsingSpec" $ do
    describe "name" $ do
      let parse = Megaparsec.parse name ""

      it "succeeds on a valid name" $
        parse "name" `shouldParse` Name "name"
      
      it "fails on an invalid name" $
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
        parse "name: type" `shouldParse`
          Binding (Name "name") (ExVariable (Name "type"))
      
      it "fails on an invalid binding" $
        parse `shouldFailOn` "name:"
    
    describe "bindings" $ do
      let parse = Megaparsec.parse bindings ""

      it "succeeds on a valid binding sequence" $
        parse "name: type, name: type," `shouldParse`
          [Binding (Name "name") (ExVariable (Name "type"))
          ,Binding (Name "name") (ExVariable (Name "type"))
          ]
    
    describe "expression" $ do
      let parse = Megaparsec.parse expression ""
      
      it "succeeds on a pi abstraction" $
        parse "[name: type, name: type, type]" `shouldParse`
          ExPiAbstraction
            [Binding (Name "name") (ExVariable (Name "type"))
            ,Binding (Name "name") (ExVariable (Name "type"))
            ]
            (ExVariable (Name "type"))
      
      it "fails on an invalid pi abstraction" $
        parse `shouldFailOn` "[type, name: type, type,]"
      
      it "succeeds on a lambda abstraction" $
        parse "{name: type, name: type, name}" `shouldParse`
          ExLambdaAbstraction
            [Binding (Name "name") (ExVariable (Name "type"))
            ,Binding (Name "name") (ExVariable (Name "type"))
            ]
            (ExVariable (Name "name"))
      
      it "fails on an invalid lambda abstraction" $
        parse `shouldFailOn` "{name, name: type, name,}"

      it "succeeds on an application" $
        parse "(function first-argument second-argument)" `shouldParse`
          ExApplication
            (ExVariable (Name "function"))
            [ExVariable (Name "first-argument")
            ,ExVariable (Name "second-argument")
            ]

    describe "statement" $ do
      let parse = Megaparsec.parse statement ""
      
      it "succeeds on a definition" $
        parse "def identity {a: type, value: a, value} end" `shouldParse`
          StDef (Name "identity")
            (ExLambdaAbstraction
              [Binding (Name "a") (ExVariable (Name "type"))
              ,Binding (Name "value") (ExVariable (Name "a"))
              ]
              (ExVariable (Name "value"))
            )
