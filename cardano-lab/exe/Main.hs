{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Lab (NodeHandle (..), mkNodeHandle)
import Prelude
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let networkId = Testnet (NetworkMagic 2)
      nodeSocket = "/tmp"
      stateDirectory = "/tmp"
      action = pure ()
  NodeHandle{startNode, stopNode} <- mkNodeHandle networkId nodeSocket stateDirectory action
  nodeHandle <- startNode
  threadDelay 5000000
  stopNode nodeHandle
  pure ()
