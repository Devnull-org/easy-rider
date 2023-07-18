module Cardano.Mithril where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Prelude hiding (getContents)
import Control.Lens ((^?))
import Control.Monad.Free (Free (..), foldFree)
import Data.Aeson.Lens (key, nth, _String)
import qualified Data.Text as T
import GHC.Base (String, error)
import GHC.IO.Handle (hGetLine, hIsEOF, hIsOpen)
import System.Process (CreateProcess (std_err, std_out), StdStream (..), proc, readCreateProcess, terminateProcess, withCreateProcess)

newtype MithrilF next
  = DownloadSnapshot next
  deriving (Functor)

type Mithril = Free MithrilF

downloadSnapshot' :: Mithril ()
downloadSnapshot' = Free $ DownloadSnapshot (Pure ())

mithrilProgram :: Mithril ()
mithrilProgram = downloadSnapshot'

interpretMithrilIO :: Mithril a -> IO a
interpretMithrilIO = foldFree go
 where
  go :: MithrilF a -> IO a
  go (DownloadSnapshot next) = do
    listAndDownloadLastSnapshot
    pure next

mithril :: IO ()
mithril = interpretMithrilIO mithrilProgram

data MithrilArguments = MithrilArguments
  { miNetworkId :: NetworkId
  , miNodeSocket :: FilePath
  , miStateDirectory :: FilePath
  }
  deriving (Eq, Show)

defaultMithrilArguments :: MithrilArguments
defaultMithrilArguments =
  MithrilArguments
    { miNetworkId = Testnet (NetworkMagic 2)
    , miNodeSocket = "/tmp"
    , miStateDirectory = "/tmp"
    }

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
    \_stdin mout _stderr processHandle -> do
      let delaySeconds :: Int = 2
      out <- waitForHandle delaySeconds mout
      processLines out processHandle
 where
  waitForHandle n mhandle =
    if isNothing mhandle
      then do
        threadDelay 1000000
        waitForHandle (n - 1) mhandle
      else case mhandle of
        -- TODO: throw concrete exception here
        Nothing -> error "Handle not writeable"
        Just out -> pure out

  processLines out processHandle = do
    isOpen <- hIsOpen out
    if isOpen
      then do
        end <- hIsEOF out
        if end
          then terminateProcess processHandle
          else do
            line <- hGetLine out
            putStrLn line
      -- TODO: throw concrete exception here
      else error "Handle not opened"

  downloadSnapshotCmd sn =
    [ "--run-mode"
    , "preview"
    , "--config-directory"
    , "cardano-lab/config/mithril/network"
    , "snapshot"
    , "download"
    , sn
    ]
