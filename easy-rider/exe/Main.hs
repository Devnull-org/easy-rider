{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Cardano.Mithril
import Cardano.Node
import Options
import System.Directory (doesDirectoryExist)

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
      dbExists <- doesDirectoryExist "./db"
      if dbExists
        then runCardanoNode nodeArguments
        else do
          listAndDownloadLastSnapshot networkId
          runCardanoNode nodeArguments
