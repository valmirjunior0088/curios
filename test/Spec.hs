import qualified Curios.ParsingSpec (spec)
import qualified Curios.TranslationSpec (spec)
import Test.Hspec (hspec)

main =
  hspec $ do
    Curios.ParsingSpec.spec
    Curios.TranslationSpec.spec
