module Cardano.Util where

import Cardano.Prelude
import System.Process (ProcessHandle, waitForProcess)
import Prelude (error)

newtype ProcessDied = ProcessDied Text deriving (Eq, Show)

instance Exception ProcessDied

checkProcessHasFinished :: Text -> ProcessHandle -> IO ()
checkProcessHasFinished name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> pure ()
    ExitFailure _ -> throwIO $ ProcessDied name
