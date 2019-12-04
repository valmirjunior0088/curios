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
    [ "(package examples"
    , "  (define id <a:type. a. a>"
    , "    {a. value. value}"
    , "  )"
    , ""
    , "  (package pair"
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
( StPackage (Name "examples")
  ( Program
    [ StDefine (Name "id")
      ( ExPiAbstraction
        [ PiBinding (Just (Name "a")) (ExVariable (Identifier [Name "type"]))
        , PiBinding Nothing (ExVariable (Identifier [Name "a"]))
        ]
        ( ExVariable (Identifier [Name "a"]) )
      )
      ( ExLambdaAbstraction
        [ LambdaBinding (Name "a") Nothing
        , LambdaBinding (Name "value") Nothing
        ]
        ( ExVariable (Identifier [Name "value"]) )
      )
    , StPackage (Name "pair")
      ( Program
        [ StDefine (Name "pair")
          ( ExPiAbstraction
            [ PiBinding Nothing (ExVariable (Identifier [Name "type"]))
            , PiBinding Nothing (ExVariable (Identifier [Name "type"]))
            ]
            ( ExVariable (Identifier [Name "type"]) )
          )
          ( ExLambdaAbstraction
            [ LambdaBinding (Name "a") Nothing
            , LambdaBinding (Name "b") Nothing
            ]
            ( ExPiAbstraction
              [ PiBinding (Just (Name "c")) (ExVariable (Identifier [Name "type"]))
              , PiBinding Nothing
                ( ExPiAbstraction
                  [ PiBinding Nothing (ExVariable (Identifier [Name "a"]))
                  , PiBinding Nothing (ExVariable (Identifier [Name "b"]))
                  ]
                  ( ExVariable (Identifier [Name "c"]) )
                )
              ]
              ( ExVariable (Identifier [Name "c"]) )
            )
          )
        , StDefine (Name "make")
          ( ExPiAbstraction
            [ PiBinding (Just (Name "a")) (ExVariable (Identifier [Name "type"]))
            , PiBinding (Just (Name "b")) (ExVariable (Identifier [Name "type"]))
            , PiBinding Nothing (ExVariable (Identifier [Name "a"]))
            , PiBinding Nothing (ExVariable (Identifier [Name "b"]))
            ]
            ( ExApplication
              ( ExVariable (Identifier [Name "pair"]))
              [ ExVariable (Identifier [Name "a"])
              , ExVariable (Identifier [Name "b"])
              ]
            )
          )
          ( ExLambdaAbstraction
            [ LambdaBinding (Name "a") Nothing
            , LambdaBinding (Name "b") Nothing
            , LambdaBinding (Name "x") Nothing
            , LambdaBinding (Name "y") Nothing
            ]
            ( ExLambdaAbstraction
              [ LambdaBinding (Name "c") Nothing
              , LambdaBinding (Name "f") Nothing
              ]
              ( ExApplication
                ( ExVariable (Identifier [Name "f"]))
                [ ExVariable (Identifier [Name "x"])
                , ExVariable (Identifier [Name "y"])
                ]
              )
            )
          )
        ]
      )
    ]
  )
)
-}
