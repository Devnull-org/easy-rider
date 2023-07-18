module Lab where

import Cardano.Prelude

import Cardano.Node (NodeArguments, runCardanoNode, queryTipSlotNo)
import Control.Monad.Free (Free (..), foldFree)
import Control.Concurrent.Class.MonadSTM (TQueue, MonadSTM (readTQueue))

data CardanoNodeF next
  = Start (Async () -> next)
  | QueryTip next
  | Stop (Async ()) next

deriving instance Functor CardanoNodeF

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode (Async ())
startTheNode = Free $ Start Pure

queryNodeTip :: CardanoNode () 
queryNodeTip = Free $ QueryTip (Pure ()) 

stopTheNode :: Async () -> CardanoNode () 
stopTheNode a = Free $ Stop a (Pure ())

program :: CardanoNode ()
program = do
  res <- startTheNode
  queryNodeTip
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
  go (QueryTip next) = do
      tipSlot <- queryTipSlotNo na
      print tipSlot
      pure next
  go (Stop asyncHandle next) = do
    cancel asyncHandle
    pure next
