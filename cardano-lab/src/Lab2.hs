{-# LANGUAGE QuasiQuotes #-}

module Lab2 where

import Cardano.Prelude

import qualified Cardano.Mithril as Mithril
import qualified Cardano.Node as Node
import Data.Text (unpack)
import Text.RawString.QQ

-- * Mithil typeclass
class Monad m => Mithril m where
  downloadSnapshot :: Text -> m ()

instance Mithril IO where
  downloadSnapshot = Mithril.downloadSnapshot . unpack

-- * Cardano Node

class Monad m => CardanoNode m where
  runCardanoNode :: Node.NodeArguments -> m ()

instance CardanoNode IO where
  runCardanoNode = Node.runCardanoNode

interpretCardanoNodeIO :: CardanoNode IO => Node.NodeArguments -> IO ()
interpretCardanoNodeIO = runCardanoNode

-- * Program

class Monad m => Command m where
  startTheNode :: Node.AvailableNetworks -> m ()
  unknownCommand :: m ()

class (Command m, Monad m) => Program m where
  displaySplash :: m ()
  displayPrompt :: m ()
  getUserInput :: m ()
  parseUserInput :: m ()
  handleCommand :: m ()

-- programIO :: Program a -> IO a
-- programIO prog =
--   case runFree prog of
--     Pure x -> return x
--     Free (DisplaySplash next) -> do
--       putText splash
--       programIO $ next splash
--     Free (DisplayPrompt next) -> do
--       putText prompt
--       programIO $ next prompt
--     Free (GetInput next) -> do
--       x <- getLine
--       programIO $ next x
--     Free (ParseInput t next) ->
--       case readMaybe t :: Maybe AvailableNetworks of
--         Just network -> programIO $ next (StartTheNode network)
--         Nothing -> programIO $ next UnknownCommand
--     Free (HandleCommand c next) ->
--       case c of
--         StartTheNode network -> do
--           let na =
--                 NodeArguments
--                   { naNetworkId = toNetworkId network
--                   , naNodeSocket = "./."
--                   }
--           dbExists <- doesDirectoryExist "db"
--           if dbExists
--             then do
--               _ <- runCardanoNode na
--               programIO $ next c
--             else do
--               listAndDownloadLastSnapshot
--               _ <- runCardanoNode na
--               programIO $ next c
--         UnknownCommand -> do
--           putStrLn ("Unknown command. Please try again." :: Text)
--           programIO $ next c
--
-- -- | Simpler version of the program
-- program' :: FreeT CardanoNode Mithril ()
-- program' = do
--   _ <- lift mithrilProgram
--   liftF startTheNode
--
-- interpretIO' :: NodeArguments -> FreeT CardanoNode Mithril a -> IO a
-- interpretIO' na prog = do
--   x <- interpretMithrilIO $ runFreeT prog
--   case x of
--     Pure a -> return a
--     Free a -> do
--       next <- interpretCardanoNodeIO na a
--       interpretIO' na next

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
                                       
> Description: Start cardano-node on the specified network using mithril-client
> to download the latest snapshot. 
> mithril-client will download the latest snapshot for specified network into the "db"
> directory in the root of the project.
> If this directory is not empty it is the user responsibility to make sure 
> each subsequent run of this app is using the same network identifier or delete
> the folder before running the cardano-lab app.
>
>  
> Please choose the cardano-node network: 
> Preview / Preprod / Mainnet ?
  |]
