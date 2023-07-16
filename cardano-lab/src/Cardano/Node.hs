module Cardano.Node where

import Cardano.Api (
  CardanoMode,
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
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
  connectToLocalNode,
 )
import Cardano.Prelude (Word64, traceShow, unlessM, whenM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Exception (finally)
import Data.Text (Text)
import Data.Void (Void)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, waitForProcess, withCreateProcess)
import Prelude

data NodeArguments = NodeArguments
  { naNetworkId :: NetworkId
  , naNodeSocket :: FilePath
  , naStateDirectory :: FilePath
  }
  deriving (Eq, Show)

defaultNodeArguments :: NodeArguments
defaultNodeArguments =
  NodeArguments
    { naNetworkId = Testnet (NetworkMagic 2)
    , naNodeSocket = "/tmp"
    , naStateDirectory = "/tmp"
    }

data NodeHandle a = NodeHandle
  { startNode :: IO (Async a)
  , stopNode :: Async a -> IO ()
  }

mkNodeHandle ::
  NodeArguments ->
  IO (NodeHandle Int)
mkNodeHandle na = do
  let startNode = async $ withCardanoNode na
  let stopNode = cancel
  pure $ NodeHandle startNode stopNode

runCardanoNode :: NodeArguments -> IO Int
runCardanoNode = withCardanoNode

withCardanoNode ::
  NodeArguments ->
  IO Int
withCardanoNode NodeArguments{naNetworkId, naNodeSocket, naStateDirectory} = do
  p <- process
  withCreateProcess p{std_out = Inherit, std_err = Inherit} $
    \_stdin _stdout _stderr processHandle ->
      ( race
          (checkProcessHasNotDied "cardano-node" processHandle)
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
        (Just naStateDirectory)
        (defaultCardanoNodeArgs $ networkIdToNodeConfigPath cwd naNetworkId)

  socketPath = naStateDirectory </> naNodeSocket

  waitForNode = do
    waitForSocket naNodeSocket
    pure 1

  cleanupSocketFile =
    whenM (doesFileExist socketPath) $
      removeFile socketPath

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  defaultByronEpochSlots = 21600 :: Word64

networkIdToNodeConfigPath :: FilePath -> NetworkId -> FilePath
networkIdToNodeConfigPath cwd network =
  let basePath = cwd </> "cardano-lab" </> "config" </> "cardano-configurations" </> "network"
   in case network of
        Mainnet -> basePath </> "mainnet" </> "cardano-node"
        Testnet (NetworkMagic 1) -> basePath </> "preprod" </> "cardano-node"
        Testnet (NetworkMagic 2) -> basePath </> "preview" </> "cardano-node"
        Testnet (NetworkMagic 1097911063) -> basePath </> "testnet" </> "cardano-node"
        _ -> error "TODO: implement running on devnet"

-- | Wait for the node socket file to become available.
waitForSocket :: FilePath -> IO ()
waitForSocket nodeSocket =
  unlessM (doesFileExist nodeSocket) $ do
    threadDelay 1
    waitForSocket nodeSocket

type Port = Int

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: FilePath
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

connectCardanoNode :: NetworkId -> FilePath -> IO ()
connectCardanoNode networkId nodeSocket =
  connectToLocalNode connectInfo clientProtocols
 where
  connectInfo =
    LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

  clientProtocols =
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> error "Process has died"
    ExitFailure exit -> error $ "Process " <> show name <> " exited with failure code: " <> show exit
