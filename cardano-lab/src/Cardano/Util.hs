module Cardano.Util where

import Cardano.Api (
  CardanoMode,
  ChainTip (..),
  ConsensusModeParams (..),
  EpochSlots (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo,
  SocketPath,
  getLocalChainTip,
 )
import Cardano.Prelude
import Data.String (String)
import GHC.Base (error)
import System.Directory (doesFileExist)
import System.Process (ProcessHandle, waitForProcess)

newtype ProcessDied = ProcessDied Text deriving (Eq, Show)

instance Exception ProcessDied

type NodeSocket = FilePath

-- TODO: move this to dedicated cardano-node module
data NodeArguments = NodeArguments
  { naNetworkId :: NetworkId
  , naNodeSocket :: FilePath
  , naPreventOutput :: Bool
  }
  deriving (Eq, Show)

-- TODO: move this to dedicated hydra module
data HydraNodeArguments = HydraNodeArguments
  { hnNetworkId :: NetworkId
  , hnNodeSocket :: FilePath
  , hnPreventOutput :: Bool
  }
  deriving (Eq, Show)

checkProcessHasFinished :: Text -> ProcessHandle -> IO ()
checkProcessHasFinished name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> pure ()
    ExitFailure _ -> throwIO $ ProcessDied name

-- | Wait for the node socket file to become available.
waitForSocket :: NodeSocket -> IO ()
waitForSocket nodeSocket =
  unlessM (doesFileExist nodeSocket) $ do
    threadDelay 1
    waitForSocket nodeSocket

-- | Query the latest chain point just for the slot number.
queryTipSlotNo :: HydraNodeArguments -> IO SlotNo
queryTipSlotNo HydraNodeArguments{hnNetworkId, hnNodeSocket} =
  getLocalChainTip (localNodeConnectInfo hnNetworkId (File hnNodeSocket)) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

localNodeConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  defaultByronEpochSlots = 21600 :: Word64

networkIdToString :: NetworkId -> String
networkIdToString =
  \case
    Cardano.Api.Mainnet -> "mainnet"
    Testnet (NetworkMagic 1) -> "preprod"
    Testnet (NetworkMagic 2) -> "preview"
    -- TODO: throw real exception here
    _ -> error "Can't parse network id to string"

networkIdToArg :: NetworkId -> [String]
networkIdToArg =
  \case
    Cardano.Api.Mainnet -> ["mainnet"]
    Testnet (NetworkMagic 1) -> ["testnet-magic", "1"]
    Testnet (NetworkMagic 2) -> ["testnet-magic", "2"]
    -- TODO: throw real exception here
    _ -> error "Can't parse network id to string"
