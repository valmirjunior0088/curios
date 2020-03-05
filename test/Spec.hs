import qualified Curios.ParsingSpec (spec)
import Test.Hspec (hspec)

main =
  hspec $ do
    Curios.ParsingSpec.spec
