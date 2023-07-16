module Cardano.Util where

import Cardano.Prelude
import System.Process (ProcessHandle, waitForProcess)
import Prelude (error)

checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> error "Process has died"
    ExitFailure exit -> error $ "Process " <> show name <> " exited with failure code: " <> show exit
