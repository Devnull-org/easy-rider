module Cardano.Hydra where

import Cardano.Prelude hiding (getContents)
import Cardano.Util (checkProcessHasFinished)
import GHC.IO.Handle (hGetLine)
import System.Process (CreateProcess (std_err, std_out), StdStream (..), proc, withCreateProcess)

runHydra :: IO ()
runHydra = do
  generateCardanoKeys
  generateHydraKey
  publishHydraScripts
  let hydraProc = proc "hydra-node" runHydraCmd
  withCreateProcess hydraProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr processHandle ->
      race_
        (checkProcessHasFinished "hydra-node" processHandle)
        (outputLines mout)
 where
  runHydraCmd =
    [ "--node-id"
    , "easy-rider-node-1"
    , "--ledger-protocol-parameters"
    , "easy-rider/config/protocol-parameters.json"
    , "--node-socket"
    , "db/node.socket"
    , "--testnet-magic"
    , "2"
    ]

generateHydraKey :: IO ()
generateHydraKey = do
  let hydraProc = proc "hydra-node" runHydraCmd
  withCreateProcess hydraProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr processHandle ->
      race_
        (checkProcessHasFinished "hydra-node" processHandle)
        (outputLines mout)
 where
  runHydraCmd =
    [ "gen-hydra-key"
    , "--output-file"
    , "hydra"
    ]

publishHydraScripts :: IO ()
publishHydraScripts = do
  let hydraProc = proc "hydra-node" runHydraCmd
  withCreateProcess hydraProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr processHandle ->
      race_
        (checkProcessHasFinished "hydra-node" processHandle)
        (outputLines mout)
 where
  runHydraCmd =
    [ "publish-scripts"
    , "--node-socket"
    , "db/node.socket"
    , "--cardano-signing-key"
    , "cardano.sk"
    , "--testnet-magic"
    , "2"
    ]

generateCardanoKeys :: IO ()
generateCardanoKeys = do
  let hydraProc = proc "cardano-cli" runCardanoCliCmd
  withCreateProcess hydraProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr processHandle ->
      race_
        (checkProcessHasFinished "cardano-cli" processHandle)
        (outputLines mout)
 where
  runCardanoCliCmd =
    [ "address"
    , "key-gen"
    , "--signing-key-file"
    , "cardano.sk"
    , "--verification-key-file"
    , "cardano.vk"
    ]

outputLines :: Maybe Handle -> IO ()
outputLines mout = do
  let delaySeconds :: Int = 2
  out <- waitForHandle delaySeconds mout
  processLines out

waitForHandle :: Num t => t -> Maybe b -> IO b
waitForHandle n mhandle =
  case mhandle of
    Nothing -> do
      threadDelay 1000000
      waitForHandle (n - 1) mhandle
    Just out ->
      pure out

processLines :: Handle -> IO ()
processLines out = do
  line <- hGetLine out
  putStrLn line
