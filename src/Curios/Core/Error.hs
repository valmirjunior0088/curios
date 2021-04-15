module Curios.Core.Error
  (Kind (..)
  ,Error (..)
  ,throw
  ,showError
  )
  where

import Curios.Core (Origin (..), Name, Type, trShow)
import Curios.Core.Variables (Variables, vrDepth)
import Curios.PrettyPrinting.Megaparsec (showSource)

data Kind =
  KnMismatchedFunctionType Type |
  KnMismatchedType Type Type |
  KnUndeclaredName Name |
  KnNonInferable

data Error =
  Error { erOrigin :: Origin, erVariables :: Variables, erKind :: Kind }

throw :: Origin -> Variables -> Kind -> Either Error a
throw origin variables kind =
  Left (Error { erOrigin = origin, erVariables = variables, erKind = kind })

showKind :: (Type -> String) -> Kind -> String
showKind showType kind =
  case kind of
    KnMismatchedFunctionType obtained ->
      "Type mismatch" ++ "\n"
        ++ "- Expected: <function type>" ++ "\n"
        ++ "- Obtained: " ++ showType obtained ++ "\n"
    KnMismatchedType expected obtained ->
      "Type mismatch" ++ "\n"
        ++ "- Expected: " ++ showType expected ++ "\n"
        ++ "- Obtained: " ++ showType obtained ++ "\n"
    KnUndeclaredName name ->
      "The name \"" ++ name ++ "\" is undeclared" ++ "\n"
    KnNonInferable ->
      "The term does not have an inferable type without an annotation" ++ "\n"

showOrigin ::  String -> Origin -> String
showOrigin source origin =
  case origin of
    OrMachine ->
      "In a machine-generated term..." ++ "\n"
    OrSource sourcePos ->
      showSource sourcePos source

showError :: String -> Error -> String
showError source (Error { erVariables, erOrigin, erKind }) =
  showOrigin source erOrigin
    ++ "\n"
    ++ showKind showType erKind
  where
    showType termType = trShow (vrDepth erVariables) termType
