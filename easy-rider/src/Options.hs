module Options where

import Cardano.Prelude

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.String (String)
import Options.Applicative
import Options.Applicative.Help (vsep)

newtype NodeOptions = NodeOptions
  { networkId :: NetworkId
  }
  deriving (Eq, Show)

newtype Command
  = RunCardanoNode NodeOptions
  deriving (Show, Eq)

parseCommand :: IO Command
parseCommand = getArgs <&> parseCommandFromArgs >>= handleParseResult

parseCommandFromArgs :: [String] -> ParserResult Command
parseCommandFromArgs = execParserPure defaultPrefs easyRiderCommand

easyRiderCommand :: ParserInfo Command
easyRiderCommand =
  info
    ( commandParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Easy Rider"
        <> header "Makes it easy to build on Cardano TM"
    )

commandParser :: Parser Command
commandParser =
  asum
    [ RunCardanoNode . NodeOptions <$> cardanoNodeParser
    ]
 where
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
