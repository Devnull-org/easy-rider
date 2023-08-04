{-# language QuasiQuotes #-}
module Lab where

import Cardano.Prelude

import Cardano.Mithril (listAndDownloadLastSnapshot)
import Cardano.Node (NodeArguments, runCardanoNode)
import Control.Concurrent.Class.MonadSTM (TQueue)
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
      -- NOTE: it can happen that db directory exists but the network on
      -- subsequent runs are different which will cause problems.
      dbExists <- doesDirectoryExist "db"
      if dbExists
        then interpretMithrilIO next
        else do
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

interpretCardanoNodeIO :: NodeArguments -> TQueue IO Text -> CardanoNode a -> IO a
interpretCardanoNodeIO na queue prog =
  case runFree prog of
    Pure a -> return a
    Free (Start next) -> do
      runCardanoNode na queue
      interpretCardanoNodeIO na queue next

-- * Program

data Command
        = StartTheNode 
        | UnknownCommand
        deriving (Eq, Show)

data ProgramF next
  = DisplayPrompt (Text -> next) 
  | GetInput (Text -> next)
  | ParseInput Text (Command -> next) 
  | DisplayCommand Command (Command -> next)
  deriving (Functor)


type Program = Free ProgramF

displayPrompt :: Program Text
displayPrompt = liftF $ DisplayPrompt id

getInput :: Program Text
getInput = liftF $ GetInput id

parseInput :: Text -> Program Command 
parseInput t = liftF $ ParseInput t id 

displayCommand :: Command -> Program Command 
displayCommand c = liftF $ DisplayCommand c id 

program' :: Program Command
program' = do
 void displayPrompt 
 input <- getInput
 command <- parseInput input
 displayCommand command 

programIO' :: Program a -> IO a 
programIO' prog =
  case runFree prog of
    Pure x -> return x 
    Free (DisplayPrompt next) -> do
        putText prompt
        programIO' $ next prompt
    Free (GetInput next) -> do
      x <- getLine
      programIO' $ next x 
    Free (ParseInput t next) -> 
      case t of
        "1" -> programIO' $ next StartTheNode 
        _ -> programIO' $ next UnknownCommand 
    Free (DisplayCommand c next) -> 
        case c of
         StartTheNode -> do 
             putStrLn ("start cardano-node here ..." :: Text)
             programIO' $ next c
         UnknownCommand -> do 
             putStrLn ("Unknown command. Please try again." :: Text)
             programIO' $ next c


program :: FreeT CardanoNode Mithril ()
program = do
  _ <- lift mithrilProgram
  liftF startTheNode

interpretIO :: NodeArguments -> TQueue IO Text -> FreeT CardanoNode Mithril a -> IO a
interpretIO na queue prog = do
  x <- interpretMithrilIO $ runFreeT prog
  case x of
    Pure a -> return a
    Free a -> do
      next <- interpretCardanoNodeIO na queue a
      interpretIO na queue next


prompt :: Text
prompt = 
  [r|
   ______               __                     __          __  
  / ____/___ __________/ /___ _____  ____     / /   ____ _/ /_ 
 / /   / __ `/ ___/ __  / __ `/ __ \/ __ \   / /   / __ `/ __ \
/ /___/ /_/ / /  / /_/ / /_/ / / / / /_/ /  / /___/ /_/ / /_/ /
\____/\__,_/_/   \__,_/\__,_/_/ /_/\____/  /_____/\__,_/_.___/ 
                                       
> 
> Commands: 
> 1. Start cardano-node
> Waiting for command...
>>
  |]
                                                              
                                                              
