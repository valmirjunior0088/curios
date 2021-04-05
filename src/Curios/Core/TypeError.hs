module Curios.Core.TypeError
  (TypeError (..)
  ,teUndeclaredName
  ,teMismatchedFunctionType
  ,teMismatchedType
  ,teNonInferable
  ,showTypeError
  )
  where

import Curios.Core.Term (Origin (..), Name, Type)
import Curios.Formatting (framed)

data Kind =
  KnMismatchedFunctionType Type |
  KnMismatchedType Type Type |
  KnUndeclaredName Name |
  KnNonInferable

data TypeError =
  TypeError { teOrigin :: Origin, teKind :: Kind }

teMismatchedFunctionType :: Origin -> Type -> TypeError
teMismatchedFunctionType origin obtained =
  TypeError { teOrigin = origin, teKind = KnMismatchedFunctionType obtained }

teMismatchedType :: Origin -> Type -> Type -> TypeError
teMismatchedType origin expected obtained =
  TypeError { teOrigin = origin, teKind = KnMismatchedType expected obtained }

teUndeclaredName :: Origin -> Name -> TypeError
teUndeclaredName origin name =
  TypeError { teOrigin = origin, teKind = KnUndeclaredName name }

teNonInferable :: Origin -> TypeError
teNonInferable origin =
  TypeError { teOrigin = origin, teKind = KnNonInferable }

showOrigin :: Origin -> String -> String
showOrigin origin source =
  case origin of
    OrMachine ->
      "In a machine-generated term..." ++ "\n"
    OrSource sourcePos ->
      framed sourcePos source

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

showTypeError :: TypeError -> String -> String
showTypeError typeError source =
  showOrigin (teOrigin typeError) source
    ++ "\n"
    ++ showKind (teKind typeError)
