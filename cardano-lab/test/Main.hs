module Main where

import Prelude
import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig Spec.spec
