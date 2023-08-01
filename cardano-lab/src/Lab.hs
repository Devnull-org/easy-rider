module Lab where

import Cardano.Prelude

import Cardano.Mithril (listAndDownloadLastSnapshot)
import Cardano.Node (NodeArguments, runCardanoNode)
import Control.Concurrent.Class.MonadSTM (TQueue)
import Control.Monad.Trans.Free (Free, FreeF (..), FreeT (..), liftF, runFree)
import GHC.Base (id)
import System.Directory (doesDirectoryExist)

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

data ProgramF next
  = GetInput (Text -> next)
  | StopProgram
  deriving (Functor)

type Program = Free ProgramF

getInput :: Program Text
getInput = liftF $ GetInput id

stopProgram :: Program a
stopProgram = liftF StopProgram

program :: FreeT CardanoNode Mithril ()
program = do
  _ <- lift mithrilProgram
  liftF startTheNode

interpretIO :: NodeArguments -> TQueue IO Text -> FreeT CardanoNode Mithril a -> IO a
interpretIO na queue prog = do
  r <- interpretMithrilIO $ runFreeT prog
  case r of
    Pure x -> return x
    Free a -> do
      next <- interpretCardanoNodeIO na queue a
      interpretIO na queue next
