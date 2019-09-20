import Curios.Parsing
  ( statement
  )

import Text.Megaparsec
  ( parseTest
  )

main :: IO ()
main = parseTest statement example

example :: String
example =
  unlines [
    "module either where",
    "  assume either : <type! type! type>",
    "  assume left : <a:type! b:type. (either a b)>",
    "  assume right : <a:type. b:type! (either a b)>",
    "  define elim : <unknown! unknown> = {unknown! unknown}",
    "end"
  ]

{-
StProgram
  (Program "either"
    [StAssume "either"
      (ExPiAbstraction
        [Quantifier Nothing (ExVariable (Identifier ["type"])) AvExplicit
        ,Quantifier Nothing (ExVariable (Identifier ["type"])) AvExplicit
        ]
        (ExVariable (Identifier ["type"]))
      )
    ,StAssume "left"
      (ExPiAbstraction
        [Quantifier (Just "a") (ExVariable (Identifier ["type"])) AvExplicit
        ,Quantifier (Just "b") (ExVariable (Identifier ["type"])) AvImplicit
        ]
        (ExApplication
          (ExVariable (Identifier ["either"]))
          [Argument (ExVariable (Identifier ["a"])) Nothing
          ,Argument (ExVariable (Identifier ["b"])) Nothing
          ]
        )
      )
    ,StAssume "right"
      (ExPiAbstraction
        [Quantifier (Just "a") (ExVariable (Identifier ["type"])) AvImplicit
        ,Quantifier (Just "b") (ExVariable (Identifier ["type"])) AvExplicit
        ]
        (ExApplication
          (ExVariable (Identifier ["either"]))
          [Argument (ExVariable (Identifier ["a"])) Nothing
          ,Argument (ExVariable (Identifier ["b"])) Nothing
          ]
        )
      )
    ,StDefine "elim"
      (ExPiAbstraction
        [Quantifier Nothing (ExVariable (Identifier ["unknown"])) AvExplicit]
        (ExVariable (Identifier ["unknown"]))
      )
      (ExLambdaAbstraction
        [Binding "unknown" Nothing AvExplicit]
        (ExVariable (Identifier ["unknown"]))
      )
    ]
  )
-}
