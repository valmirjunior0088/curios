module Curios.Elaboration.Statement
  (pgDeclarations
  ,pgDefinitions
  )
  where

import Curios.Source (Identifier (..), Binding (..), Statement (..), Program (..))
import Curios.Core (Origin (..), Term (..), trApplyVariable)
import Curios.Elaboration.Expression (exTranslate)

trAbstractDeclarationBinding :: Binding -> Term -> Term
trAbstractDeclarationBinding (Binding sourcePos (Identifier _ name) expression) term =
  TrFunctionType (OrSource sourcePos) (exTranslate expression) output where
    output _ input = trApplyVariable name input term

pgDeclarations :: Program -> [(Identifier, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (StDefn _ identifier bindings declaration _) =
      (identifier, foldr trAbstractDeclarationBinding (exTranslate declaration) bindings)

trAbstractDefinitionBinding :: Binding -> Term -> Term
trAbstractDefinitionBinding (Binding sourcePos (Identifier _ name) _) term =
  TrFunction (OrSource sourcePos) output where
    output input = trApplyVariable name input term

pgDefinitions :: Program -> [(Identifier, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (StDefn _ identifier bindings _ definition) =
      (identifier, foldr trAbstractDefinitionBinding (exTranslate definition) bindings)
