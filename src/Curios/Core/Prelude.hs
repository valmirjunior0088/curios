module Curios.Core.Prelude
  (Prelude
  ,prelude
  ,prDefinitions
  ,prDeclarations
  )
  where

import Curios.Core.Term
  (Literal (..)
  ,Name
  ,Type
  ,Term (..)
  ,trPrText
  ,trPrInteger
  ,trPrReal
  ,trLtText
  ,trLtInteger
  ,trLtReal
  ,trOpUnary
  ,trOpBinary
  ,trReference
  ,trType
  ,trFunctionType
  ,trFunction
  ,trApplication
  ,vrUnwrap
  )

data Entry =
  Entry
    {enName :: Name
    ,enType :: Type
    ,enTerm :: Term
    }

boolean :: Entry
boolean =
  Entry
    {enName = "Boolean"
    ,enType = trType
    ,enTerm =
      trFunctionType
        (trFunctionType (trReference "Boolean") (\_ _ -> trType))
        (\self input ->
          (trFunctionType (trApplication (vrUnwrap input) (trReference "true"))
            (\_ _ ->
              (trFunctionType (trApplication (vrUnwrap input) (trReference "false"))
                (\_ _ ->
                  (trApplication (vrUnwrap input) (vrUnwrap self))
                )
              )
            )
          )
        )
    }

booleanTrue :: Entry
booleanTrue =
  Entry
    {enName = "true"
    ,enType = trReference "Boolean"
    ,enTerm = trFunction (\_ -> trFunction (\pTrue -> trFunction (\_ -> vrUnwrap pTrue)))
    }

booleanFalse :: Entry
booleanFalse =
  Entry
    {enName = "false"
    ,enType = trReference "Boolean"
    ,enTerm = trFunction (\_ -> trFunction (\_ -> trFunction (\pFalse -> vrUnwrap pFalse)))
    }

textLength :: Entry
textLength =
  Entry
    {enName = "~~"
    ,enType = trFunctionType trPrText (\_ _ -> trPrInteger)
    ,enTerm = trOpUnary "~~" (\(LtText text) -> trLtInteger (length text))
    } 

textConcatenate :: Entry
textConcatenate =
  Entry
    {enName = "++"
    ,enType = trFunctionType trPrText (\_ _ -> trFunctionType trPrText (\_ _ -> trPrText))
    ,enTerm = trOpBinary "++" (\(LtText one) (LtText another) -> trLtText (one ++ another))
    }

integerSum :: Entry
integerSum =
  Entry
    {enName = "+"
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger))
    ,enTerm = trOpBinary "+" (\(LtInteger one) (LtInteger another) -> trLtInteger (one + another))
    }

integerSubtract :: Entry
integerSubtract =
  Entry
    {enName = "-"
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger))
    ,enTerm = trOpBinary "-" (\(LtInteger one) (LtInteger another) -> trLtInteger (one - another))
    }

integerMultiply :: Entry
integerMultiply =
  Entry
    {enName = "*"
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger))
    ,enTerm = trOpBinary "*" (\(LtInteger one) (LtInteger another) -> trLtInteger (one * another))
    }

integerEqualTo :: Entry
integerEqualTo =
  Entry
    {enName = "="
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean"))
    ,enTerm =
      trOpBinary "=" (\(LtInteger one) (LtInteger other) -> trReference (if one == other then "true" else "false"))
    }

integerLesserThan :: Entry
integerLesserThan =
  Entry
    {enName = "<"
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean"))
    ,enTerm =
      trOpBinary "<" (\(LtInteger one) (LtInteger other) -> trReference (if one < other then "true" else "false"))
    }

integerLesserThanOrEqualTo :: Entry
integerLesserThanOrEqualTo =
  Entry
    {enName = "<="
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean"))
    ,enTerm =
      trOpBinary "<=" (\(LtInteger one) (LtInteger other) -> trReference (if one <= other then "true" else "false"))
    }

integerGreaterThan :: Entry
integerGreaterThan =
  Entry
    {enName = ">"
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean"))
    ,enTerm =
      trOpBinary ">" (\(LtInteger one) (LtInteger other) -> trReference (if one > other then "true" else "false"))
    }

integerGreaterThanOrEqualTo :: Entry
integerGreaterThanOrEqualTo =
  Entry
    {enName = ">="
    ,enType = trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean"))
    ,enTerm =
      trOpBinary ">=" (\(LtInteger one) (LtInteger other) -> trReference (if one >= other then "true" else "false"))
    }

realSum :: Entry
realSum =
  Entry
    {enName = "+."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal))
    ,enTerm = trOpBinary "+." (\(LtReal one) (LtReal another) -> trLtReal (one + another))
    }

realSubtract :: Entry
realSubtract =
  Entry
    {enName = "-."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal))
    ,enTerm = trOpBinary "-." (\(LtReal one) (LtReal another) -> trLtReal (one - another))
    }

realMultiply :: Entry
realMultiply =
  Entry
    {enName = "*."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal))
    ,enTerm = trOpBinary "*." (\(LtReal one) (LtReal another) -> trLtReal (one * another))
    }

realDivide :: Entry
realDivide =
  Entry
    {enName = "/."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal))
    ,enTerm = trOpBinary "/." (\(LtReal one) (LtReal another) -> trLtReal (one / another))
    }

realEqualTo :: Entry
realEqualTo =
  Entry
    {enName = "=."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean"))
    ,enTerm = trOpBinary "=." (\(LtReal one) (LtReal other) -> trReference (if one == other then "true" else "false"))
    }

realLesserThan :: Entry
realLesserThan =
  Entry
    {enName = "<."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean"))
    ,enTerm = trOpBinary "<." (\(LtReal one) (LtReal other) -> trReference (if one < other then "true" else "false"))
    }

realLesserThanOrEqualTo :: Entry
realLesserThanOrEqualTo =
  Entry
    {enName = "<=."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean"))
    ,enTerm = trOpBinary "<=." (\(LtReal one) (LtReal other) -> trReference (if one <= other then "true" else "false"))
    }

realGreaterThan :: Entry
realGreaterThan =
  Entry
    {enName = ">."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean"))
    ,enTerm = trOpBinary ">." (\(LtReal one) (LtReal other) -> trReference (if one > other then "true" else "false"))
    }

realGreaterThanOrEqualTo :: Entry
realGreaterThanOrEqualTo =
  Entry
    {enName = ">=."
    ,enType = trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean"))
    ,enTerm = trOpBinary ">=." (\(LtReal one) (LtReal other) -> trReference (if one >= other then "true" else "false"))
    }

type Prelude =
  [Entry]

prelude :: Prelude
prelude =
  [boolean
  ,booleanTrue
  ,booleanFalse
  ,textLength
  ,textConcatenate
  ,integerSum
  ,integerSubtract
  ,integerMultiply
  ,integerEqualTo
  ,integerLesserThan
  ,integerLesserThanOrEqualTo
  ,integerGreaterThan
  ,integerGreaterThanOrEqualTo
  ,realSum
  ,realSubtract
  ,realMultiply
  ,realDivide
  ,realEqualTo
  ,realLesserThan
  ,realLesserThanOrEqualTo
  ,realGreaterThan
  ,realGreaterThanOrEqualTo
  ]

prDeclarations :: Prelude -> [(Name, Term)]
prDeclarations entries =
  map transform entries where
    transform entry = (enName entry, enType entry)

prDefinitions :: Prelude -> [(Name, Term)]
prDefinitions entries =
  map transform entries where
    transform entry = (enName entry, enTerm entry)
