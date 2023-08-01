module Lab where

import Cardano.Prelude

import Cardano.Mithril (listAndDownloadLastSnapshot)
import Cardano.Node (NodeArguments, runCardanoNode)
import Control.Concurrent.Class.MonadSTM (TQueue)
import Control.Monad.Trans.Free (Free, FreeF (..), FreeT (..), liftF, runFree)
import GHC.Base (id)

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

data CardanoNodeF next
  = Start (Async () -> next)
  | Stop (Async ()) next

deriving instance Functor CardanoNodeF

type CardanoNode = Free CardanoNodeF

startTheNode :: CardanoNode (Async ())
startTheNode = liftF $ Start id

stopTheNode :: Async () -> CardanoNode ()
stopTheNode a = liftF $ Stop a ()

cardanoNodeProgram :: CardanoNode ()
cardanoNodeProgram = do
  res <- startTheNode
  stopTheNode res

interpretCardanoNodeIO :: NodeArguments -> TQueue IO Text -> CardanoNode a -> IO a
interpretCardanoNodeIO na queue prog =
  case runFree prog of
    Pure a -> return a
    Free (Start next) -> do
      r <- async $ runCardanoNode na queue
      interpretCardanoNodeIO na queue $ next r
    Free (Stop asyncHandle next) -> do
      cancel asyncHandle
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

-- program :: Program a
program = do 
  i <- getInput
  case i of
    "1" -> do 
       lift mithrilProgram 
       getInput 
    -- "2" -> do 
    --    asyncHandle <- liftF startTheNode 
    --    undefined
    "stop" -> stopProgram 
  
   

programIO :: NodeArguments -> TQueue IO Text -> Program a -> IO a
programIO na queue prog = 
  case runFree prog of
    Pure x -> return x
    Free (GetInput next) -> do
      line <- getLine
      case line of
        "1" -> do
           interpretMithrilIO mithrilProgram
           programIO na queue $ next "1"
        "2" -> do 
          asyncHandle <- interpretCardanoNodeIO na queue startTheNode
          programIO na queue $ next "2"
        x -> programIO na queue $ next

-- program :: FreeT CardanoNode Mithril ()
-- program = do
--   _ <- lift mithrilProgram
--   asyncHandle <- liftF startTheNode
--   liftF $ stopTheNode asyncHandle

-- interpretIO :: NodeArguments -> TQueue IO Text -> FreeT CardanoNode Mithril a -> IO a
-- interpretIO na queue prog = do
--   r <- interpretMithrilIO $ runFreeT prog
--   case r of
--     Pure x -> return x
--     Free a -> do
--       next <- interpretCardanoNodeIO na queue a
--       interpretIO na queue next
