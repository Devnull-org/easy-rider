{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Cardano.Mithril
import Cardano.Node
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
              }

      listAndDownloadLastSnapshot networkId
      runCardanoNode nodeArguments
