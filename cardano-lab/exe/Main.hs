{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Mithril (mithril)
import Cardano.Node (defaultNodeArguments)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM (MonadSTM (newTQueue), atomically)
import Control.Monad (forM_)
import Lab (interpret, program)

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
  -- queue <- atomically newTQueue
  -- result <- interpret defaultNodeArguments queue program
  -- print result
  mithril
