module Lab where

import Cardano.Prelude

import Cardano.Node (NodeArguments, runCardanoNode)
import Control.Monad.Free (Free (..), foldFree)

data CardanoNodeF next
  = Start (Async Int -> next)
  | Stop (Async Int) next
  deriving (Functor)

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode (Async Int)
startTheNode = Free $ Start Pure

stopTheNode :: Async Int -> CardanoNode ()
stopTheNode a = Free $ Stop a (Pure ())

program :: CardanoNode ()
program = do
  res <- startTheNode
  stopTheNode res

interpret :: NodeArguments -> CardanoNode a -> IO a
interpret na = foldFree go
 where
  go :: CardanoNodeF a -> IO a
  go (Start next) = do
    r <- async $ runCardanoNode na
    pure $ next r
  go (Stop asyncHandle next) = do
    cancel asyncHandle
    pure next
