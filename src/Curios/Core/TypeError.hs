module Curios.Core.TypeError
  (Kind (..)
  ,TypeError (..)
  ,showTypeError
  )
  where

import Curios.Core (Origin (..), Name, Type)
import Curios.PrettyPrinting.Framed (framed)

data Kind =
  KnMismatchedFunctionType Type |
  KnMismatchedType Type Type |
  KnUndeclaredName Name |
  KnNonInferable

data TypeError =
  TypeError { teOrigin :: Origin, teKind :: Kind }

showKind :: Kind -> String
showKind kind =
  case kind of
    KnMismatchedFunctionType obtained ->
      "Type mismatch" ++ "\n"
        ++ "- Expected: <function type>" ++ "\n"
        ++ "- Obtained: " ++ show obtained ++ "\n"
    KnMismatchedType expected obtained ->
      "Type mismatch" ++ "\n"
        ++ "- Expected: " ++ show expected ++ "\n"
        ++ "- Obtained: " ++ show obtained ++ "\n"
    KnUndeclaredName name ->
      "The name \"" ++ name ++ "\" is undeclared" ++ "\n"
    KnNonInferable ->
      "The term does not have an inferable type without an annotation" ++ "\n"

showOrigin :: Origin -> String -> String
showOrigin origin source =
  case origin of
    OrMachine ->
      "In a machine-generated term..." ++ "\n"
    OrSource sourcePos ->
      framed sourcePos source

showTypeError :: TypeError -> String -> String -> String
showTypeError typeError name source =
  showOrigin (teOrigin typeError) source
    ++ "\n"
    ++ "When processing \"" ++ name ++ "\": " ++ showKind (teKind typeError)
