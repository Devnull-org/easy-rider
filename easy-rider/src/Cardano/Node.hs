module Cardano.Node where

import Cardano.Api (
  CardanoMode,
  ChainTip (ChainTip, ChainTipAtGenesis),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  File (File),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (
    LocalNodeClientProtocols,
    localChainSyncClient,
    localStateQueryClient,
    localTxMonitoringClient,
    localTxSubmissionClient
  ),
  LocalNodeConnectInfo (
    LocalNodeConnectInfo,
    localConsensusModeParams,
    localNodeNetworkId,
    localNodeSocketPath
  ),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  SlotNo,
  SocketPath,
  connectToLocalNode,
  getLocalChainTip,
 )
import Cardano.Prelude
import Cardano.Util (checkProcessHasFinished)
import Data.String (String)
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (..), proc, withCreateProcess)
import Prelude (error)

data NodeArguments = NodeArguments
  { naNetworkId :: NetworkId
  , naNodeSocket :: FilePath
  }
  deriving (Eq, Show)

data NodeHandle a = NodeHandle
  { startNode :: IO (Async a)
  , stopNode :: Async a -> IO ()
  }

data AvailableNetworks
  = Preview
  | Preprod
  | Mainnet
  deriving (Eq, Show, Read)

type Port = Int
type NodeSocket = FilePath

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: NodeSocket
  , nodeConfigFile :: FilePath
  , nodeByronGenesisFile :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile :: FilePath
  , nodeTopologyFile :: FilePath
  , nodeDatabaseDir :: FilePath
  , nodeDlgCertFile :: Maybe FilePath
  , nodeSignKeyFile :: Maybe FilePath
  , nodeOpCertFile :: Maybe FilePath
  , nodeKesKeyFile :: Maybe FilePath
  , nodeVrfKeyFile :: Maybe FilePath
  , nodePort :: Maybe Port
  }

mkNodeHandle ::
  NodeArguments ->
  IO (NodeHandle ())
mkNodeHandle na = do
  let startNode = async $ withCardanoNode na
  let stopNode = cancel
  pure $ NodeHandle startNode stopNode

runCardanoNode :: NodeArguments -> IO ()
runCardanoNode = withCardanoNode

withCardanoNode ::
  NodeArguments ->
  IO ()
withCardanoNode NodeArguments{naNetworkId, naNodeSocket} = do
  p <- process
  withCreateProcess p{std_out = Inherit, std_err = Inherit} $
    \_stdin _stdout _stderr processHandle ->
      ( race
          (checkProcessHasFinished "cardano-node" processHandle)
          waitForNode
          >>= \case
            Left{} -> error "never should have been reached"
            Right a -> pure a
      )
        `finally` cleanupSocketFile
 where
  process = do
    cwd <- getCurrentDirectory
    pure $
      cardanoNodeProcess
        (Just "db")
        (defaultCardanoNodeArgs $ networkIdToNodeConfigPath cwd naNetworkId)
  waitForNode = do
    waitForSocket naNodeSocket
    pure ()

  cleanupSocketFile =
    whenM (doesFileExist naNodeSocket) $
      removeFile naNodeSocket

-- | Query the latest chain point just for the slot number.
queryTipSlotNo :: NodeArguments -> IO SlotNo
queryTipSlotNo NodeArguments{naNetworkId, naNodeSocket} =
  getLocalChainTip (localNodeConnectInfo naNetworkId (File naNodeSocket)) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

localNodeConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  defaultByronEpochSlots = 21600 :: Word64

networkIdToNodeConfigPath :: FilePath -> NetworkId -> FilePath
networkIdToNodeConfigPath cwd network =
  let basePath = cwd </> "easy-rider" </> "config" </> "cardano-configurations" </> "network"
   in case network of
        Cardano.Api.Mainnet -> basePath </> "mainnet" </> "cardano-node"
        Testnet (NetworkMagic 1) -> basePath </> "preprod" </> "cardano-node"
        Testnet (NetworkMagic 2) -> basePath </> "preview" </> "cardano-node"
        Testnet (NetworkMagic 1097911063) -> basePath </> "testnet" </> "cardano-node"
        _ -> error "TODO: implement running on devnet"

toNetworkId :: AvailableNetworks -> NetworkId
toNetworkId =
  \case
    Cardano.Node.Mainnet -> Cardano.Api.Mainnet
    Cardano.Node.Preview -> Testnet (NetworkMagic 1)
    Cardano.Node.Preprod -> Testnet (NetworkMagic 2)

networkIdToString :: NetworkId -> String
networkIdToString =
  \case
    Cardano.Api.Mainnet -> "mainnet"
    Testnet (NetworkMagic 1) -> "preprod"
    Testnet (NetworkMagic 2) -> "preview"
    -- TODO: throw real exception here
    _ -> error "Can't parse network id to string"

-- | Wait for the node socket file to become available.
waitForSocket :: NodeSocket -> IO ()
waitForSocket nodeSocket =
  unlessM (doesFileExist nodeSocket) $ do
    threadDelay 1
    waitForSocket nodeSocket

defaultCardanoNodeArgs :: FilePath -> CardanoNodeArgs
defaultCardanoNodeArgs nodeConfigPath =
  CardanoNodeArgs
    { nodeSocket = "node.socket"
    , nodeConfigFile = nodeConfigPath </> "config.json"
    , nodeByronGenesisFile = "genesis-byron.json"
    , nodeShelleyGenesisFile = "genesis-shelley.json"
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
    , nodeTopologyFile = nodeConfigPath </> "topology.json"
    , nodeDatabaseDir = "db"
    , nodeDlgCertFile = Nothing
    , nodeSignKeyFile = Nothing
    , nodeOpCertFile = Nothing
    , nodeKesKeyFile = Nothing
    , nodeVrfKeyFile = Nothing
    , nodePort = Nothing
    }

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args =
  (proc "cardano-node" strArgs){cwd}
 where
  CardanoNodeArgs
    { nodeConfigFile
    , nodeTopologyFile
    , nodeDatabaseDir
    , nodeSocket
    , nodePort
    , nodeSignKeyFile
    , nodeDlgCertFile
    , nodeOpCertFile
    , nodeKesKeyFile
    , nodeVrfKeyFile
    } = args

  strArgs =
    "run"
      : mconcat
        [ ["--config", nodeConfigFile]
        , ["--topology", nodeTopologyFile]
        , ["--database-path", nodeDatabaseDir]
        , ["--socket-path", nodeSocket]
        , opt "--port" (show <$> nodePort)
        , opt "--byron-signing-key" nodeSignKeyFile
        , opt "--byron-delegation-certificate" nodeDlgCertFile
        , opt "--shelley-operational-certificate" nodeOpCertFile
        , opt "--shelley-kes-key" nodeKesKeyFile
        , opt "--shelley-vrf-key" nodeVrfKeyFile
        ]

  opt :: a -> Maybe a -> [a]
  opt arg = \case
    Nothing -> []
    Just val -> [arg, val]

connectCardanoNode :: NetworkId -> NodeSocket -> IO ()
connectCardanoNode networkId nodeSocket =
  connectToLocalNode connectInfo clientProtocols
 where
  connectInfo =
    LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = File nodeSocket
      }

  clientProtocols =
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
