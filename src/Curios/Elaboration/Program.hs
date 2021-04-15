module Curios.Elaboration.Program
  (pgDeclarations
  ,pgDefinitions
  )
  where

import Curios.Source (Identifier (..), Binding (..), Statement (..), Program (..))
import Curios.Core (Origin (..), Name, Term (..), trApplyVariable)
import Curios.Elaboration.Expression (exTranslate)
import Text.Megaparsec (SourcePos)

trAbstractDeclarationBinding :: Binding -> Term -> Term
trAbstractDeclarationBinding (Binding sourcePos (Identifier _ name) expression) term =
  TrFunctionType (OrSource sourcePos) (exTranslate expression) output where
    output _ input = trApplyVariable name input term

pgDeclarations :: Program -> [(SourcePos, Name, Term)]
pgDeclarations (Program _ program) =
  map transform program where
    transform (StDefn sourcePos (Identifier _ name) bindings declaration _) =
      (sourcePos, name, foldr trAbstractDeclarationBinding (exTranslate declaration) bindings)

trAbstractDefinitionBinding :: Binding -> Term -> Term
trAbstractDefinitionBinding (Binding sourcePos (Identifier _ name) _) term =
  TrFunction (OrSource sourcePos) output where
    output input = trApplyVariable name input term

pgDefinitions :: Program -> [(SourcePos, Name, Term)]
pgDefinitions (Program _ program) =
  map transform program where
    transform (StDefn sourcePos (Identifier _ name) bindings _ definition) =
      (sourcePos, name, foldr trAbstractDefinitionBinding (exTranslate definition) bindings)
