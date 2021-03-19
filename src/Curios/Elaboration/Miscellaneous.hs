module Curios.Elaboration.Miscellaneous
  (idTranslate
  ,ltTranslate
  )
  where

import Curios.Source.Types (Identifier (..))
import Curios.Core.Term (Origin (..), Primitive (..), Term (..))
import Text.Megaparsec (SourcePos)
import qualified Curios.Source.Types as Source
import qualified Curios.Core.Term as Core

idTranslate :: SourcePos -> Identifier -> Term
idTranslate sourcePos identifier =
  case identifier of
    Identifier _ "Type" -> TrType (OrSource sourcePos)
    Identifier _ "Text" -> TrPrimitive (OrSource sourcePos) PrText
    Identifier _ "Integer" -> TrPrimitive (OrSource sourcePos) PrInteger
    Identifier _ "Real" -> TrPrimitive (OrSource sourcePos) PrReal
    Identifier _ name -> TrReference (OrSource sourcePos) name

ltTranslate :: SourcePos -> Source.Literal -> Term
ltTranslate sourcePos literal =
  case literal of
    Source.LtText _ string -> TrLiteral (OrSource sourcePos) (Core.LtText string)
    Source.LtInteger _ integer -> TrLiteral (OrSource sourcePos) (Core.LtInteger integer)
    Source.LtReal _ double -> TrLiteral (OrSource sourcePos) (Core.LtReal double)
