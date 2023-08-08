{-# LANGUAGE QuasiQuotes #-}

module Lab where

import Cardano.Prelude

import Cardano.Mithril (listAndDownloadLastSnapshot)
import Cardano.Node (AvailableNetworks, NodeArguments (..), runCardanoNode, toNetworkId)
import Control.Monad.Trans.Free (Free, FreeF (..), FreeT (..), liftF, runFree)
import GHC.Base (id)
import System.Directory (doesDirectoryExist)
import Text.RawString.QQ

-- * Mithil

newtype MithrilF next
  = DownloadSnapshot next
  deriving (Functor)

type Mithril = Free MithrilF

downloadSnapshot' :: Mithril ()
downloadSnapshot' = liftF $ DownloadSnapshot ()

mithrilProgram :: Mithril ()
mithrilProgram = downloadSnapshot'

interpretMithrilIO :: Mithril a -> IO a
interpretMithrilIO prog =
  case runFree prog of
    Pure a -> return a
    Free (DownloadSnapshot next) -> do
      listAndDownloadLastSnapshot
      interpretMithrilIO next

-- * Cardano Node

newtype CardanoNodeF next
  = Start next

deriving instance Functor CardanoNodeF

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode ()
startTheNode = liftF $ Start ()

cardanoNodeProgram :: CardanoNode ()
cardanoNodeProgram = startTheNode

interpretCardanoNodeIO :: NodeArguments -> CardanoNode a -> IO a
interpretCardanoNodeIO na prog =
  case runFree prog of
    Pure a -> return a
    Free (Start next) -> do
      runCardanoNode na
      interpretCardanoNodeIO na next

-- * Program

data Command
  = StartTheNode AvailableNetworks
  | UnknownCommand
  deriving (Eq, Show)

data ProgramF next
  = DisplaySplash (Text -> next)
  | DisplayPrompt (Text -> next)
  | GetInput (Text -> next)
  | ParseInput Text (Command -> next)
  | HandleCommand Command (Command -> next)
  deriving (Functor)

type Program = Free ProgramF

displaySplash :: Program Text
displaySplash = liftF $ DisplaySplash id

displayPrompt :: Program Text
displayPrompt = liftF $ DisplayPrompt id

getInput :: Program Text
getInput = liftF $ GetInput id

parseInput :: Text -> Program Command
parseInput t = liftF $ ParseInput t id

handleCommand :: Command -> Program Command
handleCommand c = liftF $ HandleCommand c id

program :: Program Command
program = do
  void displaySplash
  go
 where
  go = do
    void displayPrompt
    input <- getInput
    command <- parseInput input
    _ <- handleCommand command
    go

programIO :: Program a -> IO a
programIO prog =
  case runFree prog of
    Pure x -> return x
    Free (DisplaySplash next) -> do
      putText splash
      programIO $ next splash
    Free (DisplayPrompt next) -> do
      putText prompt
      programIO $ next prompt
    Free (GetInput next) -> do
      x <- getLine
      programIO $ next x
    Free (ParseInput t next) ->
      case readMaybe t :: Maybe AvailableNetworks of
        Just network -> programIO $ next (StartTheNode network)
        Nothing -> programIO $ next UnknownCommand
    Free (HandleCommand c next) ->
      case c of
        StartTheNode network -> do
          let na =
                NodeArguments
                  { naNetworkId = toNetworkId network
                  , naNodeSocket = "./."
                  }
          dbExists <- doesDirectoryExist "db"
          if dbExists
            then do
              _ <- runCardanoNode na
              programIO $ next c
            else do
              listAndDownloadLastSnapshot
              _ <- runCardanoNode na
              programIO $ next c
        UnknownCommand -> do
          putStrLn ("Unknown command. Please try again." :: Text)
          programIO $ next c

-- | Simpler version of the program
program' :: FreeT CardanoNode Mithril ()
program' = do
  _ <- lift mithrilProgram
  liftF startTheNode

interpretIO' :: NodeArguments -> FreeT CardanoNode Mithril a -> IO a
interpretIO' na prog = do
  x <- interpretMithrilIO $ runFreeT prog
  case x of
    Pure a -> return a
    Free a -> do
      next <- interpretCardanoNodeIO na a
      interpretIO' na next

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
