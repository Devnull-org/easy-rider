module Cardano.Mithril where

import Cardano.Api (NetworkId)
import Cardano.Node (networkIdToString)
import Cardano.Prelude hiding (getContents)
import Cardano.Util (checkProcessHasFinished)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth, _String)
import qualified Data.Text as T
import GHC.Base (String, error)
import GHC.IO.Handle (hGetLine)
import System.Process (CreateProcess (std_err, std_out), StdStream (..), proc, readCreateProcess, withCreateProcess)

listAndDownloadLastSnapshot :: NetworkId -> IO ()
listAndDownloadLastSnapshot networkId = do
  let mithrilProc = proc "mithril-client" listSnapshotsArgs
  snapshotsJson <- readCreateProcess mithrilProc ""
  case snapshotsJson ^? nth 0 . key "digest" . _String of
    -- TODO: throw concrete exception here
    Nothing -> error "Could not get the last snapshot digest"
    Just snapshot -> downloadSnapshot networkId $ T.unpack snapshot
 where
  listSnapshotsArgs =
    [ "--run-mode"
    , networkIdToString networkId
    , "--config-directory"
    , "easy-rider/config/mithril/network"
    , "snapshot"
    , "list"
    , "--json"
    ]

downloadSnapshot :: NetworkId -> String -> IO ()
downloadSnapshot networkId snapshot = do
  let mithrilProc = proc "mithril-client" (downloadSnapshotCmd snapshot)
  withCreateProcess mithrilProc{std_out = Inherit, std_err = Inherit} $
    \_stdin mout _stderr processHandle ->
      race_
        (checkProcessHasFinished "mithril-client" processHandle)
        (outputLines mout)
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
    , networkIdToString networkId
    , "--config-directory"
    , "easy-rider/config/mithril/network"
    , "snapshot"
    , "download"
    , sn
    ]
