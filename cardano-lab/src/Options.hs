module Options where

import Cardano.Prelude

import Cardano.Api (NetworkId (..), NetworkMagic (..), SlotNo (..))
import Data.String (String)
import Options.Applicative
import Options.Applicative.Help (vsep)

newtype NodeOptions = NodeOptions
  { networkId :: NetworkId
  }
  deriving (Eq, Show)

data HydraNodeOptions = HydraNodeOptions
  { hydraNetworkId :: NetworkId
  , startAtSlot :: SlotNo
  }
  deriving (Eq, Show)

data Command
  = RunCardanoNode NodeOptions
  | RunHydraNode HydraNodeOptions
  deriving (Show, Eq)

parseCommand :: IO Command
parseCommand = getArgs <&> parseCommandFromArgs >>= handleParseResult

parseCommandFromArgs :: [String] -> ParserResult Command
parseCommandFromArgs = execParserPure defaultPrefs cardanoLabCommand

cardanoLabCommand :: ParserInfo Command
cardanoLabCommand =
  info
    ( commandParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Cardano Lab"
        <> header "Makes it easy to build on Cardano TM"
    )

commandParser :: Parser Command
commandParser =
  asum
    [ RunCardanoNode . NodeOptions <$> cardanoNodeParser
    , RunHydraNode <$> hydraNodeParser
    ]

hydraNodeParser :: Parser HydraNodeOptions
hydraNodeParser = HydraNodeOptions <$> hydraNodeParser' <*> slotNoParser

slotNoParser :: Parser SlotNo
slotNoParser =
  SlotNo
    <$> option
      auto
      ( long "slot-number"
          <> metavar "SLOT"
          <> help desc
      )
 where
  desc =
    "Slot Number that tells hydra-node at which slot exactly can we consider cardano-node synced \
    \and hydra-node ready to start. This is very tied to the network you are running on \
    \so the best thing is to go to one of the explorers and note the last slot of the network \
    \you want to run."

hydraNodeParser' :: Parser NetworkId
hydraNodeParser' =
  subparser $
    command
      "hail-hydra"
      ( info
          (helper <*> networkIdParser)
          ( fullDesc
              <> progDescDoc
                ( Just $
                    vsep
                      [ "Run Hydra node on the specified network"
                      , "This command spawns cardano-node using mithril behind the scenes"
                      , "to download the latest snapshot for the provided network"
                      ]
                )
          )
      )

cardanoNodeParser :: Parser NetworkId
cardanoNodeParser =
  subparser $
    command
      "run-node"
      ( info
          (helper <*> networkIdParser)
          ( fullDesc
              <> progDescDoc
                ( Just $
                    vsep
                      [ "Run Cardano node on the specified network"
                      , "This command uses mithril behind the scenes"
                      , "to download the latest snapshot for the provided network"
                      ]
                )
          )
      )

networkIdParser :: Parser NetworkId
networkIdParser = pMainnet <|> fmap Testnet pTestnetMagic
 where
  pMainnet :: Parser NetworkId
  pMainnet =
    flag'
      Mainnet
      ( long "mainnet"
          <> help "Use the mainnet magic id."
      )

  pTestnetMagic :: Parser NetworkMagic
  pTestnetMagic =
    NetworkMagic
      <$> option
        auto
        ( long "testnet-magic"
            <> metavar "NATURAL"
            <> value 42
            <> showDefault
            <> completer (listCompleter ["1", "2", "42"])
            <> help
              "Network identifier for a testnet to connect to. We only need to \
              \provide the magic number here. For example: '2' is the 'preview' \
              \network. See https://book.world.dev.cardano.org/environments.html for available networks."
        )
