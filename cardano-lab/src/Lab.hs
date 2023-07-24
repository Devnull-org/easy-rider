module Lab where

import Cardano.Prelude

import Cardano.Mithril (listAndDownloadLastSnapshot)
import Cardano.Node (NodeArguments, runCardanoNode)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue), TQueue)
import Control.Monad.Free (Free (..), foldFree)

-- * Mithil

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

-- * Cardano Node

data CardanoNodeF next
  = Start (Async () -> next)
  | Stop (Async ()) next

deriving instance Functor CardanoNodeF

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode (Async ())
startTheNode = Free $ Start Pure

stopTheNode :: Async () -> CardanoNode ()
stopTheNode a = Free $ Stop a (Pure ())

program :: CardanoNode ()
program = do
  res <- startTheNode
  stopTheNode res

interpret :: NodeArguments -> TQueue IO Text -> CardanoNode () -> IO ()
interpret na queue = foldFree go
 where
  go :: CardanoNodeF a -> IO a
  go (Start next) = do
    r <- async $ runCardanoNode na queue
    _ <- forever $ do
      msg <- atomically $ readTQueue queue
      print msg
    pure $ next r
  go (Stop asyncHandle next) = do
    cancel asyncHandle
    pure next
