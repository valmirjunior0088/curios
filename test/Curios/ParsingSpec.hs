module Curios.ParsingSpec (spec) where

import qualified Text.Megaparsec as Megaparsec (parse)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Test.Hspec (describe, it)

import Curios.Parsing
  (name
  ,qualifiedName
  ,literal
  ,piBinding
  ,lambdaBinding
  ,abstraction
  ,expression
  ,statement
  ,program
  )

import Curios.Expression
  (Name (..)
  ,QualifiedName (..)
  ,Literal (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Abstraction (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  )

spec =
  describe "Curios.ParsingSpec" $ do
    describe "name" $ do
      let parse = Megaparsec.parse name ""

      it "succeeds on a valid name" $
        parse "name" `shouldParse` Name "name"
      
      it "fails on an invalid name" $
        parse `shouldFailOn` "@invalid"
    
    describe "qualifiedName" $ do
      let parse = Megaparsec.parse qualifiedName ""

      it "succeeds on a valid qualified name" $
        parse "first;second" `shouldParse` QualifiedName [Name "first"] (Name "second")
      
      it "fails on an invalid qualified name (1)" $
        parse `shouldFailOn` "first; second"
      
      it "fails on an invalid qualified name (2)" $
        parse `shouldFailOn` ";second"

    describe "literal" $ do
      let parse = Megaparsec.parse literal ""

      it "succeeds on a character" $
        parse "'a" `shouldParse` LiCharacter 'a'
      
      it "succeeds on a string" $
        parse "\"string\"" `shouldParse` LiString "string"
      
      it "succeeds on a positive integer" $
        parse "15" `shouldParse` LiInteger 15

      it "succeeds on a negative rational" $
        parse "-15.0" `shouldParse` LiRational (-15.0)
    
    describe "piBinding" $ do
      let parse = Megaparsec.parse piBinding ""

      it "succeeds on a valid pi binding (1)" $
        parse "name:type" `shouldParse`
          PiBinding (Just (Name "name")) (ExVariable (QualifiedName [] (Name "type")))
      
      it "succeeds on a valid pi binding (2)" $
        parse "type" `shouldParse`
          PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
    
    describe "lambdaBinding"$ do
      let parse = Megaparsec.parse lambdaBinding ""

      it "succeeds on a valid lambda binding (1)" $
        parse "name:type" `shouldParse`
          LambdaBinding (Name "name") (Just (ExVariable (QualifiedName [] (Name "type"))))
      
      it "succeeds on a valid lambda binding (2)" $
        parse "name" `shouldParse` LambdaBinding (Name "name") Nothing
    
    describe "abstraction piBinding" $ do
      let parse = Megaparsec.parse (abstraction piBinding) ""

      it "succeeds on a valid pi abstraction (1)" $
        parse "name:type. name" `shouldParse`
          Abstraction
            [PiBinding (Just (Name "name")) (ExVariable (QualifiedName [] (Name "type")))]
            (ExVariable (QualifiedName [] (Name "name")))
      
      it "succeeds on a valid pi abstraction (2)" $
        parse "type. type" `shouldParse`
          Abstraction
            [PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))]
            (ExVariable (QualifiedName [] (Name "type")))
      
      it "succeeds on a valid pi abstraction (3)" $
        parse "name:type. type. type" `shouldParse`
          Abstraction
            [PiBinding (Just (Name "name")) (ExVariable (QualifiedName [] (Name "type")))
            ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
            ]
            (ExVariable (QualifiedName [] (Name "type")))
      
      it "succeeds on a valid pi abstraction (4)" $
        parse "type. name:type. type" `shouldParse`
          Abstraction
            [PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
            ,PiBinding (Just (Name "name")) (ExVariable (QualifiedName [] (Name "type")))
            ]
            (ExVariable (QualifiedName [] (Name "type")))

      it "fails on an invalid pi abstraction" $
        parse `shouldFailOn` "type. name:type. type."
    
    describe "abstraction lambdaBinding" $ do
      let parse = Megaparsec.parse (abstraction lambdaBinding) ""

      it "succeeds on a valid lambda abstraction (1)" $
        parse "name:type. name" `shouldParse`
          Abstraction
            [LambdaBinding (Name "name") (Just (ExVariable (QualifiedName [] (Name "type"))))]
            (ExVariable (QualifiedName [] (Name "name")))
      
      it "succeeds on a valid lambda abstraction (2)" $
        parse "name. name" `shouldParse`
          Abstraction
            [LambdaBinding (Name "name") Nothing]
            (ExVariable (QualifiedName [] (Name "name")))

      it "succeeds on a valid lambda abstraction (3)" $
        parse "name:type. name. name" `shouldParse`
          Abstraction
            [LambdaBinding (Name "name") (Just (ExVariable (QualifiedName [] (Name "type"))))
            ,LambdaBinding (Name "name") Nothing
            ]
            (ExVariable (QualifiedName [] (Name "name")))
      
      it "succeeds on a valid lambda abstraction (4)" $
        parse "name. name:type. name" `shouldParse`
          Abstraction
            [LambdaBinding (Name "name") Nothing
            ,LambdaBinding (Name "name") (Just (ExVariable (QualifiedName [] (Name "type"))))
            ]
            (ExVariable (QualifiedName [] (Name "name")))
      
      it "fails on an invalid lambda abstraction" $
        parse `shouldFailOn` "name. name:type. name."
    
    describe "expression" $ do
      let parse = Megaparsec.parse expression ""

      it "succeeds on a variable" $
        parse "some-name-with;dashes" `shouldParse`
          ExVariable (QualifiedName [Name "some-name-with"] (Name "dashes"))
      
      it "succeeds on a pi abstraction" $
        parse "<type. name:type. type>" `shouldParse`
          ExPiAbstraction
            (Abstraction
              [PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
              ,PiBinding (Just (Name "name")) (ExVariable (QualifiedName [] (Name "type")))
              ]
              (ExVariable (QualifiedName [] (Name "type")))
            )
      
      it "succeeds on a lambda abstraction" $
        parse "{name. name:type. name}" `shouldParse`
          ExLambdaAbstraction
            (Abstraction
              [LambdaBinding (Name "name") Nothing
              ,LambdaBinding (Name "name") (Just (ExVariable (QualifiedName [] (Name "type"))))
              ]
              (ExVariable (QualifiedName [] (Name "name")))
            )

      it "succeeds on an application" $
        parse "[function first-argument second-argument]" `shouldParse`
          ExApplication
            (ExVariable (QualifiedName [] (Name "function")))
            [ExVariable (QualifiedName [] (Name "first-argument"))
            ,ExVariable (QualifiedName [] (Name "second-argument"))
            ]

    describe "statement" $ do
      let parse = Megaparsec.parse statement ""

      it "succeeds on an empty module" $
        parse "(module name)" `shouldParse` StModule (Name "name") (Program [])
      
      it "succeeds on a definition" $
        parse "(define identity <a:type. a. a> {a. value. value})" `shouldParse`
          StDefine (Name "identity")
            (ExPiAbstraction
              (Abstraction
                [PiBinding (Just (Name "a")) (ExVariable (QualifiedName [] (Name "type")))
                ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "a")))
                ]
                (ExVariable (QualifiedName [] (Name "a")))
              )
            )
            (ExLambdaAbstraction
              (Abstraction
                [LambdaBinding (Name "a") Nothing
                ,LambdaBinding (Name "value") Nothing
                ]
                (ExVariable (QualifiedName [] (Name "value")))
              )
            )

      it "succeeds on a module with a single definition" $
        parse "(module name (define identity <a:type. a. a> {a. value. value}))" `shouldParse`
          StModule (Name "name")
            (Program
              [StDefine (Name "identity")
                (ExPiAbstraction
                  (Abstraction
                    [PiBinding (Just (Name "a")) (ExVariable (QualifiedName [] (Name "type")))
                    ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "a")))
                    ]
                    (ExVariable (QualifiedName [] (Name "a")))
                  )
                )
                (ExLambdaAbstraction
                  (Abstraction
                    [LambdaBinding (Name "a") Nothing
                    ,LambdaBinding (Name "value") Nothing
                    ]
                    (ExVariable (QualifiedName [] (Name "value")))
                  )
                )
              ]
            )
      
      it "succeeds on a nested module" $
        parse "(module outer (module inner))" `shouldParse`
          StModule (Name "outer") (Program [StModule (Name "inner") (Program [])])

      it "succeeds on an import" $
        parse "(import some;qualified;name)" `shouldParse`
          StImport (QualifiedName [Name "some", Name "qualified"] (Name "name"))

      it "succeeds on a module with an import" $
        parse "(module name (import some;qualified;name))" `shouldParse`
          StModule (Name "name")
            (Program
              [StImport (QualifiedName [Name "some", Name "qualified"] (Name "name"))]
            )

    describe "program" $ do
      let parse = Megaparsec.parse program ""

      it "succeeds on an example source" $
        shouldParse
          (parse
            (unlines
              ["(define identity <a:type. a. a>"
              ,"  {a. value. value}"
              ,")"
              ,""
              ,"(module pair"
              ,"  (define pair <type. type. type>"
              ,"    {a. b."
              ,"      <c:type. <a. b. c>. c>"
              ,"    }"
              ,"  )"
              ,""
              ,"  (define make <a:type. b:type. a. b. [pair a b]>"
              ,"    {a. b. x. y."
              ,"      {c. f. [f x y]}"
              ,"    }"
              ,"  )"
              ,")"
              ,""
              ]
            )
          )
          (Program
            [StDefine (Name "identity")
              (ExPiAbstraction
                (Abstraction
                  [PiBinding (Just (Name "a")) (ExVariable (QualifiedName [] (Name "type")))
                  ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "a")))
                  ]
                  (ExVariable (QualifiedName [] (Name "a")))
                )
              )
              (ExLambdaAbstraction
                (Abstraction
                  [LambdaBinding (Name "a") Nothing, LambdaBinding (Name "value") Nothing]
                  (ExVariable (QualifiedName [] (Name "value")))
                )
              )
            ,StModule (Name "pair")
              (Program
                [StDefine (Name "pair")
                  (ExPiAbstraction
                    (Abstraction
                      [PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
                      ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "type")))
                      ]
                      (ExVariable (QualifiedName [] (Name "type")))
                    )
                  )
                  (ExLambdaAbstraction
                    (Abstraction
                      [LambdaBinding (Name "a") Nothing, LambdaBinding (Name "b") Nothing]
                      (ExPiAbstraction
                        (Abstraction
                          [PiBinding (Just (Name "c")) (ExVariable (QualifiedName [] (Name "type")))
                          ,PiBinding Nothing
                            (ExPiAbstraction
                              (Abstraction
                                [PiBinding Nothing (ExVariable (QualifiedName [] (Name "a")))
                                ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "b")))
                                ]
                                (ExVariable (QualifiedName [] (Name "c")))
                              )
                            )
                          ]
                          (ExVariable (QualifiedName [] (Name "c")))
                        )
                      )
                    )
                  )
                ,StDefine (Name "make")
                  (ExPiAbstraction
                    (Abstraction
                      [PiBinding (Just (Name "a")) (ExVariable (QualifiedName [] (Name "type")))
                      ,PiBinding (Just (Name "b")) (ExVariable (QualifiedName [] (Name "type")))
                      ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "a")))
                      ,PiBinding Nothing (ExVariable (QualifiedName [] (Name "b")))
                      ]
                      (ExApplication
                        (ExVariable (QualifiedName [] (Name "pair")))
                        [ExVariable (QualifiedName [] (Name "a"))
                        ,ExVariable (QualifiedName [] (Name "b"))
                        ]
                      )
                    )
                  )
                  (ExLambdaAbstraction
                    (Abstraction
                      [LambdaBinding (Name "a") Nothing
                      ,LambdaBinding (Name "b") Nothing
                      ,LambdaBinding (Name "x") Nothing
                      ,LambdaBinding (Name "y") Nothing
                      ]
                      (ExLambdaAbstraction
                        (Abstraction
                          [LambdaBinding (Name "c") Nothing
                          ,LambdaBinding (Name "f") Nothing
                          ]
                          (ExApplication
                            (ExVariable (QualifiedName [] (Name "f")))
                            [ExVariable (QualifiedName [] (Name "x"))
                            ,ExVariable (QualifiedName [] (Name "y"))
                            ]
                          )
                        )
                      )
                    )
                  )
                ]
              )
            ]
          )
