module Cardano.Mithril where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Prelude hiding (getContents)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)
import qualified Data.Text as T
import GHC.Base (String, error)
import GHC.IO.Handle (hGetLine)
import System.Process (CreateProcess (std_err, std_out), StdStream (..), proc, readCreateProcess, withCreateProcess)

listAndDownloadLastSnapshot :: IO ()
listAndDownloadLastSnapshot = do
  let mithrilProc = proc "mithril-client" listSnapshotsArgs
  snapshotsJson <- readCreateProcess mithrilProc ""
  case snapshotsJson ^? nth 0 . key "digest" . _String of
    -- TODO: throw concrete exception here
    Nothing -> error "Could not get the last snapshot digest"
    Just snapshot -> downloadSnapshot $ T.unpack snapshot
 where
  listSnapshotsArgs =
    [ "--run-mode"
    , "preview"
    , "--config-directory"
    , "cardano-lab/config/mithril/network"
    , "snapshot"
    , "list"
    , "--json"
    ]

downloadSnapshot :: String -> IO ()
downloadSnapshot snapshot = do
  let mithrilProc = proc "mithril-client" (downloadSnapshotCmd snapshot)
  withCreateProcess mithrilProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr _processHandle -> outputLines mout
 where
  outputLines mout = do
    let delaySeconds :: Int = 2
    out <- waitForHandle delaySeconds mout
    processLines out

  waitForHandle n mhandle =
    case mhandle of
      Nothing -> do
        threadDelay 1000000
        waitForHandle (n - 1) mhandle
      Just out ->
        pure out

  processLines out = do
    line <- hGetLine out
    putStrLn line

  downloadSnapshotCmd sn =
    [ "--run-mode"
    , "preview"
    , "--config-directory"
    , "cardano-lab/config/mithril/network"
    , "snapshot"
    , "download"
    , sn
    ]
