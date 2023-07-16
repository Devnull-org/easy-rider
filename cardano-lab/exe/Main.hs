module Main where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Node (defaultNodeArguments)
import Control.Concurrent (threadDelay)
import Lab (interpret, program)
import Prelude

main :: IO ()
main = do
  -- let networkId = Testnet (NetworkMagic 2)
  --     nodeSocket = "/tmp"
  --     stateDirectory = "/tmp"
  --     action = pure ()
  -- NodeHandle{startNode, stopNode} <- mkNodeHandle networkId nodeSocket stateDirectory action
  -- nodeHandle <- startNode
  -- threadDelay 5000000
  -- stopNode nodeHandle
  result <- interpret defaultNodeArguments program
  print result
