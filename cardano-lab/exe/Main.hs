{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Cardano.Hydra
import Cardano.Node
import Cardano.Util (
  HydraNodeArguments (..),
  NodeArguments (..),
  waitOnSlotNumber
 )
import Options

main :: IO ()
main = do
  command <- parseCommand
  case command of
    RunCardanoNode NodeOptions{networkId} -> do
      let nodeArguments =
            NodeArguments
              { naNetworkId = networkId
              , naNodeSocket = "./."
              , naPreventOutput = False
              }
      runCardanoNode nodeArguments
    RunHydraNode HydraNodeOptions{hydraNetworkId, startAtSlot} -> do
      let hydraNodeArguments =
            HydraNodeArguments
              { hnNetworkId = hydraNetworkId
              , hnNodeSocket = "db/node.socket"
              , hnPreventOutput = False
              }

      let nodeArguments =
            NodeArguments
              { naNetworkId = hydraNetworkId
              , naNodeSocket = "./."
              , naPreventOutput = True
              }

      void $
        concurrently
          (runCardanoNode nodeArguments)
          (waitOnSlotNumber hydraNodeArguments startAtSlot (runHydra hydraNodeArguments))
