module Curios.Core.Error
  ( Cause (..)
  , Error (..)
  , showError
  )
  where

import Curios.Core.Term (Origin, Name, Index)
import Curios.PrettyPrinting.Megaparsec (showFile, showSource)

data Cause =
  CsUndeclaredName Name |
  CsVariableOutOfBounds Index |
  CsFunctionsDontHaveAnInferableType |
  CsFunctionDidntHaveFunctionType |
  CsConstructorsDontHaveAnInferableType |
  CsConstructorDidntHaveSelfType |
  CsFunctionTypeMismatch |
  CsSelfTypeMismatch |
  CsTypeMismatch |
  CsNameAlreadyDeclared Name |
  CsNameAlreadyDefined Name |
  CsUndeclaredNameBeingDefined Name

showCause :: Cause -> String
showCause cause =
  case cause of
    CsUndeclaredName name -> "Undeclared name: " ++ name
    CsVariableOutOfBounds index -> "Variable out of bounds: " ++ show index
    CsFunctionsDontHaveAnInferableType -> "Functions don't have an inferable type"
    CsFunctionDidntHaveFunctionType -> "Function didn't have function type"
    CsConstructorsDontHaveAnInferableType -> "Constructors don't have an inferable type"
    CsConstructorDidntHaveSelfType -> "Constructor didn't have self type"
    CsFunctionTypeMismatch -> "Function type mismatch"
    CsSelfTypeMismatch -> "Self type mismatch"
    CsTypeMismatch -> "Type mismatch"
    CsNameAlreadyDeclared name -> "Name already declared: " ++ name
    CsNameAlreadyDefined name -> "Name already defined: " ++ name
    CsUndeclaredNameBeingDefined name -> "Undeclared name being defined: " ++ name

data Error =
  Error Origin Cause

showOrigin ::  String -> Origin -> String
showOrigin source origin =
  case origin of
    Nothing ->
      "In a machine-generated term..." ++ "\n"

    Just sourcePos ->
      showFile sourcePos
        ++ "\n"
        ++ showSource sourcePos source

showError :: String -> Error -> String
showError source (Error origin cause) =
  showOrigin source origin
    ++ "\n"
    ++ showCause cause
