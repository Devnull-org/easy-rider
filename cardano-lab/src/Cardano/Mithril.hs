module Cardano.Mithril where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Cardano.Prelude
import Cardano.Util (checkProcessHasNotDied)
import System.Process (CreateProcess (..), StdStream (Inherit), proc, withCreateProcess)
import Prelude (error)

data MithrilArguments = MithrilArguments
  { miNetworkId :: NetworkId
  , miNodeSocket :: FilePath
  , miStateDirectory :: FilePath
  }
  deriving (Eq, Show)

defaultMithrilArguments :: MithrilArguments
defaultMithrilArguments =
  MithrilArguments
    { miNetworkId = Testnet (NetworkMagic 2)
    , miNodeSocket = "/tmp"
    , miStateDirectory = "/tmp"
    }

withMithril ::
  MithrilArguments ->
  IO Int
withMithril ma = do
  let p = proc "mithril-client" []
  withCreateProcess p{std_out = Inherit, std_err = Inherit} $
    \_stdin _stdout _stderr processHandle ->
      race
        (checkProcessHasNotDied "mithril-client" processHandle)
        waitForStart
        >>= \case
          Left{} -> error "never should have been reached"
          Right a -> pure a
 where
  waitForStart = pure 1
