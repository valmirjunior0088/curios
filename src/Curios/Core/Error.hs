module Curios.Core.Error
  ( Kind (..)
  , Error (..)
  , showError
  )
  where

import Curios.Core.Term (Origin, Name, Index)
import Curios.PrettyPrinting.Megaparsec (showSource)

data Kind =
  KnUndeclaredName Name |
  KnVariableOutOfBounds Index |
  KnFunctionsDontHaveAnInferableType |
  KnFunctionDidntHaveFunctionType |
  KnConstructorsDontHaveAnInferableType |
  KnConstructorDidntHaveSelfType |
  KnFunctionTypeMismatch |
  KnSelfTypeMismatch |
  KnTypeMismatch |
  KnNameAlreadyDeclared Name |
  KnNameAlreadyDefined Name |
  KnUndeclaredNameBeingDefined Name

showKind :: Kind -> String
showKind kind =
  case kind of
    KnUndeclaredName name -> "Undeclared name: " ++ name
    KnVariableOutOfBounds index -> "Variable out of bounds: " ++ show index
    KnFunctionsDontHaveAnInferableType -> "Functions don't have an inferable type"
    KnFunctionDidntHaveFunctionType -> "Function didn't have function type"
    KnConstructorsDontHaveAnInferableType -> "Constructors don't have an inferable type"
    KnConstructorDidntHaveSelfType -> "Constructor didn't have self type"
    KnFunctionTypeMismatch -> "Function type mismatch"
    KnSelfTypeMismatch -> "Self type mismatch"
    KnTypeMismatch -> "Type mismatch"
    KnNameAlreadyDeclared name -> "Name already declared: " ++ name
    KnNameAlreadyDefined name -> "Name already defined: " ++ name
    KnUndeclaredNameBeingDefined name -> "Undeclared name being define: " ++ name

data Error =
  Error Origin Kind

showOrigin ::  String -> Origin -> String
showOrigin source origin =
  case origin of
    Nothing -> "In a machine-generated term...\n"
    Just sourcePos -> showSource sourcePos source

showError :: String -> Error -> String
showError source (Error origin kind) =
  showOrigin source origin
    ++ "\n"
    ++ showKind kind
