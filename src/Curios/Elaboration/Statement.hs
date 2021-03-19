module Curios.Elaboration.Statement
  (pgDeclarations
  ,pgDefinitions
  )
  where

import Curios.Source.Types (Identifier (..), Binding (..), Prefix (..), Statement (..), Program (..))
import Curios.Core.Term (Origin (..), Term (..), trApplyVariable)
import Curios.Elaboration.Expression (exTranslate)
import Text.Megaparsec (SourcePos)

trAbstractDeclarationBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDeclarationBinding sourcePos (Binding _ (Identifier _ name) expression) term =
  TrFunctionType (OrSource sourcePos) (exTranslate expression) output where
    output _ input = trApplyVariable name input term

pgDeclarations :: Program -> [(Identifier, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos bindings) declaration _) =
      (identifier, foldr (trAbstractDeclarationBinding sourcePos) (exTranslate declaration) bindings)

trAbstractDefinitionBinding :: SourcePos -> Binding -> Term -> Term
trAbstractDefinitionBinding sourcePos (Binding _ (Identifier _ name) _) term =
  TrFunction (OrSource sourcePos) output where
    output input = trApplyVariable name input term

pgDefinitions :: Program -> [(Identifier, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (Statement _ identifier (Prefix sourcePos bindings) _ definition) =
      (identifier, foldr (trAbstractDefinitionBinding sourcePos) (exTranslate definition) bindings)
