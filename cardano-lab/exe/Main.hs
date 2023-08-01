{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Cardano.Node (NodeArguments (..))
import Control.Concurrent.Class.MonadSTM (newTQueue)
import Lab (interpretIO, program)

main :: IO ()
main = do
  let nodeArguments =
        NodeArguments
          { naNetworkId = Testnet (NetworkMagic 2)
          , naNodeSocket = "/tmp"
          , naStateDirectory = "db"
          }
  queue <- atomically newTQueue
  _asyncHandle <- interpretIO nodeArguments queue program
  putStrLn ("cardano-node running..." :: Text)
