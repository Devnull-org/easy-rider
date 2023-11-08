{-# LANGUAGE QuasiQuotes #-}

module Lab3 where

import Cardano.Prelude

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import qualified Cardano.Hydra as Hydra
import qualified Cardano.Mithril as Mithril
import qualified Cardano.Node as Node
import System.Directory (doesDirectoryExist)
import Text.RawString.QQ

-- * Mithil typeclass
class Monad m => Mithril m where
  downloadSnapshot :: m ()

instance Mithril IO where
  downloadSnapshot = Mithril.listAndDownloadLastSnapshot (Testnet (NetworkMagic 2))

-- * Cardano Node

class Monad m => CardanoNode m where
  runCardanoNode :: Node.NodeArguments -> m ()

instance CardanoNode IO where
  runCardanoNode = Node.runCardanoNode

interpretCardanoNodeIO :: Node.NodeArguments -> IO ()
interpretCardanoNodeIO = runCardanoNode

-- * Hydra typeclass
class Monad m => Hydra m where
  runHydra :: m ()

instance Hydra IO where
  runHydra = Hydra.runHydra
-- * Program

class (Mithril m, CardanoNode m) => Command m where
  startTheNode :: Node.AvailableNetworks -> m ()
  unknownCommand :: m ()

class (Command m, Monad m) => Program m where
  displaySplash :: m ()
  displayPrompt :: m ()
  getUserInput :: m Text
  parseAndHandleUserInput :: Text -> m ()

instance Command IO where
  startTheNode network = do
    let na =
          Node.NodeArguments
            { Node.naNetworkId = Node.toNetworkId network
            , Node.naNodeSocket = "./."
            , Node.naPreventOutput = False
            }
    dbExists <- doesDirectoryExist "db"
    if dbExists
      then do
        interpretCardanoNodeIO na
      else do
        downloadSnapshot
        interpretCardanoNodeIO na
  unknownCommand =
    putStrLn ("Unknown command. Please try again." :: Text)

instance Program IO where
  displaySplash = putText splash
  displayPrompt = putText prompt
  getUserInput = getLine
  parseAndHandleUserInput t = case t of
    "Hydra" -> Hydra.runHydra
    _ -> maybe unknownCommand startTheNode (readMaybe t :: Maybe Node.AvailableNetworks)

programIO :: IO ()
programIO = do
  displaySplash
  displayPrompt
  t <- getUserInput
  parseAndHandleUserInput t

-- | TODO: Display nice prompt and use a lib to output to stdout in general.
prompt :: Text
prompt = " "

splash :: Text
splash =
  [r|
   ______               __                     __          __  
  / ____/___ __________/ /___ _____  ____     / /   ____ _/ /_ 
 / /   / __ `/ ___/ __  / __ `/ __ \/ __ \   / /   / __ `/ __ \
/ /___/ /_/ / /  / /_/ / /_/ / / / / /_/ /  / /___/ /_/ / /_/ /
\____/\__,_/_/   \__,_/\__,_/_/ /_/\____/  /_____/\__,_/_.___/ 
                                       
> Description: 
> - Start cardano-node on the specified network using mithril-client
>   to download the latest snapshot. 
>
>     mithril-client will download the latest snapshot for specified network into the "db"
>     directory in the root of the project.
>     If this directory is not empty it is the user responsibility to make sure 
>     each subsequent run of this app is using the same network identifier or delete
>     the folder before running the cardano-lab app.

> - Start hydra-node connected to existing instance of cardano-node  
>
>  
> Please specify the cardano-node network: 
> Preview / Preprod / Mainnet ?
> Or run hydra-node: 
> Hydra
  |]
