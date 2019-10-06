import Curios.Parsing
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
  unlines [
    "package either where",
    "  assume either : <type! type! type>",
    "  assume left : <a:type! b:type. (either a b)>",
    "  assume right : <a:type. b:type! (either a b)>",
    "  define elim : <unknown! unknown> = {unknown! unknown}",
    "end"
  ]

{-
( StPackage (Name "either")
  ( Program
    [ StAssume (Name "either")
      ( ExPiAbstraction
        [ Quantifier Nothing (ExVariable (Identifier [Name "type"])) AvExplicit
        , Quantifier Nothing (ExVariable (Identifier [Name "type"])) AvExplicit
        ]
        (ExVariable (Identifier [Name "type"]))
      )
    , StAssume (Name "left")
      ( ExPiAbstraction
        [ Quantifier (Just (Name "a")) (ExVariable (Identifier [Name "type"])) AvExplicit
        , Quantifier (Just (Name "b")) (ExVariable (Identifier [Name "type"])) AvImplicit
        ]
        ( ExApplication
          (ExVariable (Identifier [Name "either"]))
          [ Argument (ExVariable (Identifier [Name "a"])) Nothing
          , Argument (ExVariable (Identifier [Name "b"])) Nothing
          ]
        )
      )
    , StAssume (Name "right")
      ( ExPiAbstraction
        [ Quantifier (Just (Name "a")) (ExVariable (Identifier [Name "type"])) AvImplicit
        , Quantifier (Just (Name "b")) (ExVariable (Identifier [Name "type"])) AvExplicit
        ]
        ( ExApplication
          (ExVariable (Identifier [Name "either"]))
          [ Argument (ExVariable (Identifier [Name "a"])) Nothing
          , Argument (ExVariable (Identifier [Name "b"])) Nothing
          ]
        )
      )
    , StDefine (Name "elim")
      ( ExPiAbstraction
        [Quantifier Nothing (ExVariable (Identifier [Name "unknown"])) AvExplicit]
        (ExVariable (Identifier [Name "unknown"]))
      )
      ( ExLambdaAbstraction
        [Binding (Name "unknown") Nothing AvExplicit]
        (ExVariable (Identifier [Name "unknown"]))
      )
    ]
  )
)
-}
