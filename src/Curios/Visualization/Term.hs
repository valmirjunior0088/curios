module Curios.Visualization.Term
  (nmToBox
  ,prToBox
  ,ntToBox
  ,ctToBox
  ,scToBox
  ,trToBox
  ,dfToBox
  )
  where

import Curios.Visualization.Expression
  (idToBox
  ,liToBox
  )

import Curios.Term
  (Primitive (..)
  ,Constant (..)
  ,Name (..)
  ,Scope (..)
  ,Term (..)
  ,Definition (..)
  )

import Curios.Visualization.Common
  (parenthesized
  ,upwardsTab
  ,downwardsTab
  ,horizontallySeparated
  )

import GHC.Natural
  (Natural (..)
  )

import Text.PrettyPrint.Boxes
  (Box
  ,char
  ,text
  ,(<+>)
  )

nmToBox :: Name -> Box
nmToBox name =
  text (show name)

prToBox :: Primitive -> Box
prToBox primitive =
  text (show primitive)

ntToBox :: Natural -> Box
ntToBox natural =
  text (show natural)

ctToBox :: Constant -> Box
ctToBox constant =
  text (show constant)

scToBox :: Scope -> Box
scToBox (Scope output) =
  trToBox output

trToBox :: Term -> Box
trToBox term =
  case term of
    TrPrimitive primitive ->
      text "Primitive" <+> parenthesized (prToBox primitive)

    TrLiteral literal ->
      text "Literal" <+> parenthesized (liToBox literal)
    
    TrConstant constant ->
      text "Constant" <+> parenthesized (ctToBox constant)

    TrAbstractionType identifier input scope ->
      downwardsTab 'Π' (idToBox identifier <+> char ':' <+> trToBox input) (scToBox scope)

    TrAbstraction identifier input scope ->
      downwardsTab 'λ' (idToBox identifier <+> char ':' <+> trToBox input) (scToBox scope)
      
    TrApplication function argument ->
      upwardsTab '◆' (trToBox function) (trToBox argument)
    
    TrVariable name ->
      text "Variable" <+> parenthesized (nmToBox name)

dfToBox :: Definition -> Box
dfToBox (Definition identifier domain range) =
  horizontallySeparated
    (text "def" <+> text identifier)
    (horizontallySeparated (trToBox domain) (trToBox range))