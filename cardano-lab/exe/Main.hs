{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude hiding (concurrently)

import Cardano.Hydra
import Cardano.Node
import Cardano.Util (
  HydraNodeArguments (..),
  NodeArguments (..),
  waitOnSlotNumber,
 )
import Control.Concurrent.Async.Lifted (concurrently)
import Katip
import Options

main :: IO ()
main = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "Cardano-Lab" "production"
  bracket makeLogEnv closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "main"
    runKatipContextT le initialContext initialNamespace $ do
      $(logTM) InfoS "Cardano-Lab start"
      command <- lift parseCommand
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
