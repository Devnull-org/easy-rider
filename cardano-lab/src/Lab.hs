module Lab where

import Cardano.Prelude

import Cardano.Node (CardanoNodeResult (Done), NodeArguments, runCardanoNode)
import Control.Monad.Free (Free (..), foldFree)

data CardanoNodeF next
  = Start (Async CardanoNodeResult -> next)
  | Stop (Async CardanoNodeResult) next

deriving instance Functor CardanoNodeF

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode (Async CardanoNodeResult)
startTheNode = Free $ Start Pure

stopTheNode :: Async CardanoNodeResult -> CardanoNode CardanoNodeResult
stopTheNode a = Free $ Stop a (Pure Done)

program :: CardanoNode CardanoNodeResult
program = do
  res <- startTheNode
  stopTheNode res

interpret :: NodeArguments -> CardanoNode CardanoNodeResult -> IO CardanoNodeResult
interpret na = foldFree go
 where
  go :: CardanoNodeF a -> IO a
  go (Start next) = do
    r <- async $ runCardanoNode na
    pure $ next r
  go (Stop asyncHandle next) = do
    cancel asyncHandle
    pure next
