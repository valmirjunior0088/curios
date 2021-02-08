module Curios.Prelude
  (cnInitial
  )
  where

import Curios.Error (Error (..), showErrorKind)
import Data.Foldable (foldlM)
import Data.Either (either)

import Curios.Context
  (Context (..)
  ,cnEmpty
  ,cnInsertDeclaration
  ,cnInsertDefinition
  )

import Curios.Core.Term
  (Origin (..)
  ,Literal (..)
  ,Name
  ,Type
  ,arUnwrap
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
        (\selfArgument variableArgument ->
          (trFunctionType (trApplication (arUnwrap variableArgument) (trReference "true"))
            (\_ _ ->
              (trFunctionType (trApplication (arUnwrap variableArgument) (trReference "false"))
                (\_ _ ->
                  (trApplication (arUnwrap variableArgument) (arUnwrap selfArgument))
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
    ,enTerm = trFunction (\_ -> trFunction (\pTrue -> trFunction (\_ -> arUnwrap pTrue)))
    }

booleanFalse :: Entry
booleanFalse =
  Entry
    {enName = "false"
    ,enType = trReference "Boolean"
    ,enTerm = trFunction (\_ -> trFunction (\_ -> trFunction (\pFalse -> arUnwrap pFalse)))
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
  ,realSum
  ,realSubtract
  ,realMultiply
  ,realDivide
  ,integerEqualTo
  ,integerLesserThan
  ,integerLesserThanOrEqualTo
  ,integerGreaterThan
  ,integerGreaterThanOrEqualTo
  ,realEqualTo
  ,realLesserThan
  ,realLesserThanOrEqualTo
  ,realGreaterThan
  ,realGreaterThanOrEqualTo
  ]

enDeclarations :: [Entry] -> [(Name, Term)]
enDeclarations entries =
  map transform entries where
    transform entry = (enName entry, enType entry)

enDefinitions :: [Entry] -> [(Name, Term)]
enDefinitions entries =
  map transform entries where
    transform entry = (enName entry, enTerm entry)

cnInitial :: Context
cnInitial =
  either (error . (++) "Error in prelude: " . showErrorKind . erKind) id contextResult where
    combineDeclaration context (name, term) = cnInsertDeclaration OrMachine name term context
    combineDefinition context (name, term) = cnInsertDefinition OrMachine name term context
    contextResult = do
      context <- foldlM combineDeclaration cnEmpty (enDeclarations prelude)
      foldlM combineDefinition context (enDefinitions prelude)
