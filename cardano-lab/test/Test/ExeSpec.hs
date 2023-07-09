module Test.ExeSpec where

import System.Process (callProcess)
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "cardano-lib executable can be started" $
    it "should be able to call cardano-lab executable" $
      callProcess "cardano-lab" [] `shouldReturn` ()
