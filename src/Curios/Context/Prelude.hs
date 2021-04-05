module Curios.Context.Prelude
  (prelude
  ,prDeclarations
  ,prDefinitions
  )
  where

import Curios.Core
  (Literal (..)
  ,Name
  ,Type
  ,vrUnwrap
  ,Term
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
  )

data Entry =
  Entry
    {enName :: Name
    ,enType :: Type
    ,enTerm :: Term
    }

enOpUnary :: Name -> Type -> (Literal -> Term) -> Entry
enOpUnary name termType operator =
  Entry { enName = name, enType = termType, enTerm = trOpUnary name operator }

enOpBinary :: Name -> Type -> (Literal -> Literal -> Term) -> Entry
enOpBinary name termType operator =
  Entry { enName = name, enType = termType, enTerm = trOpBinary name operator }

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
  enOpUnary "~~"
    (trFunctionType trPrText (\_ _ -> trPrInteger))
    (\(LtText text) -> trLtInteger (length text))

textConcatenate :: Entry
textConcatenate =
  enOpBinary "++"
    (trFunctionType trPrText (\_ _ -> trFunctionType trPrText (\_ _ -> trPrText)))
    (\(LtText one) (LtText another) -> trLtText (one ++ another))

integerSum :: Entry
integerSum =
  enOpBinary "+"
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger)))
    (\(LtInteger one) (LtInteger another) -> trLtInteger (one + another))

integerSubtract :: Entry
integerSubtract =
  enOpBinary "-"
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger)))
    (\(LtInteger one) (LtInteger another) -> trLtInteger (one - another))

integerMultiply :: Entry
integerMultiply =
  enOpBinary "*"
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trPrInteger)))
    (\(LtInteger one) (LtInteger another) -> trLtInteger (one * another))

integerEqualTo :: Entry
integerEqualTo =
  enOpBinary "="
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean")))
    (\(LtInteger one) (LtInteger other) -> trReference (if one == other then "true" else "false"))

integerLesserThan :: Entry
integerLesserThan =
  enOpBinary "<"
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean")))
    (\(LtInteger one) (LtInteger other) -> trReference (if one < other then "true" else "false"))

integerLesserThanOrEqualTo :: Entry
integerLesserThanOrEqualTo =
  enOpBinary "<="
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean")))
    (\(LtInteger one) (LtInteger other) -> trReference (if one <= other then "true" else "false"))

integerGreaterThan :: Entry
integerGreaterThan =
  enOpBinary ">"
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean")))
    (\(LtInteger one) (LtInteger other) -> trReference (if one > other then "true" else "false"))

integerGreaterThanOrEqualTo :: Entry
integerGreaterThanOrEqualTo =
  enOpBinary ">="
    (trFunctionType trPrInteger (\_ _ -> trFunctionType trPrInteger (\_ _ -> trReference "Boolean")))
    (\(LtInteger one) (LtInteger other) -> trReference (if one >= other then "true" else "false"))

realSum :: Entry
realSum =
  enOpBinary "+'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal)))
    (\(LtReal one) (LtReal another) -> trLtReal (one + another))

realSubtract :: Entry
realSubtract =
  enOpBinary "-'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal)))
    (\(LtReal one) (LtReal another) -> trLtReal (one - another))

realMultiply :: Entry
realMultiply =
  enOpBinary "*'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal)))
    (\(LtReal one) (LtReal another) -> trLtReal (one * another))

realDivide :: Entry
realDivide =
  enOpBinary "/'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trPrReal)))
    (\(LtReal one) (LtReal another) -> trLtReal (one / another))

realEqualTo :: Entry
realEqualTo =
  enOpBinary "='"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean")))
    (\(LtReal one) (LtReal other) -> trReference (if one == other then "true" else "false"))

realLesserThan :: Entry
realLesserThan =
  enOpBinary "<'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean")))
    (\(LtReal one) (LtReal other) -> trReference (if one < other then "true" else "false"))

realLesserThanOrEqualTo :: Entry
realLesserThanOrEqualTo =
  enOpBinary "<='"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean")))
    (\(LtReal one) (LtReal other) -> trReference (if one <= other then "true" else "false"))

realGreaterThan :: Entry
realGreaterThan =
  enOpBinary ">'"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean")))
    (\(LtReal one) (LtReal other) -> trReference (if one > other then "true" else "false"))

realGreaterThanOrEqualTo :: Entry
realGreaterThanOrEqualTo =
  enOpBinary ">='"
    (trFunctionType trPrReal (\_ _ -> trFunctionType trPrReal (\_ _ -> trReference "Boolean")))
    (\(LtReal one) (LtReal other) -> trReference (if one >= other then "true" else "false"))

prelude :: [Entry]
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

prDeclarations :: [Entry] -> [(Name, Term)]
prDeclarations entries =
  map transform entries where
    transform entry = (enName entry, enType entry)

prDefinitions :: [Entry] -> [(Name, Term)]
prDefinitions entries =
  map transform entries where
    transform entry = (enName entry, enTerm entry)
