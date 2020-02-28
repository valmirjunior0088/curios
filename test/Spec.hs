import Curios.Parser
  ( statement
  )

import Text.Megaparsec
  ( parseTest
  )

main :: IO ()
main =
  parseTest statement example

example :: String
example =
  unlines
    [ "(module examples"
    , "  (define identity <a:type. a. a>"
    , "    {a. value. value}"
    , "  )"
    , ""
    , "  (module pair"
    , "    (define pair <type. type. type>"
    , "      {a. b."
    , "        <c:type. <a. b. c>. c>"
    , "      }"
    , "    )"
    , ""
    , "    (define make <a:type. b:type. a. b. [pair a b]>"
    , "      {a. b. x. y."
    , "        {c. f. [f x y]}"
    , "      }"
    , "    )"
    , "  )"
    , ")"
    , ""
    ]

{-
(StModule (Name "examples")
  (Program
    [StDefine (Name "identity")
      (ExPiAbstraction
        (Abstraction
          [PiBinding (Just (Name "a")) (ExSymbol (Identifier [Name "type"]))
          ,PiBinding Nothing (ExSymbol (Identifier [Name "a"]))
          ]
          (ExSymbol (Identifier [Name "a"]))
        )
      )
      (ExLambdaAbstraction
        (Abstraction
          [LambdaBinding (Name "a") Nothing
          ,LambdaBinding (Name "value") Nothing
          ]
          (ExSymbol (Identifier [Name "value"]))
        )
      )
    ,StModule (Name "pair")
      (Program 
        [StDefine (Name "pair")
          (ExPiAbstraction
            (Abstraction 
              [PiBinding Nothing (ExSymbol (Identifier [Name "type"]))
              ,PiBinding Nothing (ExSymbol (Identifier [Name "type"]))
              ]
              (ExSymbol (Identifier [Name "type"]))
            )
          )
          (ExLambdaAbstraction
            (Abstraction
              [LambdaBinding (Name "a") Nothing
              ,LambdaBinding (Name "b") Nothing
              ]
              (ExPiAbstraction
                (Abstraction
                  [PiBinding (Just (Name "c")) (ExSymbol (Identifier [Name "type"]))
                  ,PiBinding Nothing
                    (ExPiAbstraction
                      (Abstraction
                        [PiBinding Nothing (ExSymbol (Identifier [Name "a"]))
                        ,PiBinding Nothing (ExSymbol (Identifier [Name "b"]))
                        ]
                        (ExSymbol (Identifier [Name "c"]))
                      )
                    )
                  ]
                  (ExSymbol (Identifier [Name "c"]))
                )
              )
            )
          )
        ,StDefine (Name "make")
          (ExPiAbstraction
            (Abstraction
              [PiBinding (Just (Name "a")) (ExSymbol (Identifier [Name "type"]))
              ,PiBinding (Just (Name "b")) (ExSymbol (Identifier [Name "type"]))
              ,PiBinding Nothing (ExSymbol (Identifier [Name "a"]))
              ,PiBinding Nothing (ExSymbol (Identifier [Name "b"]))
              ]
              (ExApplication
                (ExSymbol (Identifier [Name "pair"]))
                [ExSymbol (Identifier [Name "a"])
                ,ExSymbol (Identifier [Name "b"])
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
                  (ExApplication (ExSymbol (Identifier [Name "f"]))
                    [ExSymbol (Identifier [Name "x"])
                    ,ExSymbol (Identifier [Name "y"])
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
)
-}
