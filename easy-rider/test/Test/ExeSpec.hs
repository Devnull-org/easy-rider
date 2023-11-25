module Test.ExeSpec where

import System.Process (readProcess)
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "all external executable are in place" $ do
    it "should be able to call cardano-node executable" $
      readProcess "cardano-node" ["--version"] "" >>= (`shouldContain` "8.1.2")
    it "should be able to call mithril-client executable" $
      readProcess "mithril-client" ["--version"] "" >>= (`shouldContain` "0.3.20")
