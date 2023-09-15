module Main where

import Data.Version (showVersion)
import Paths_hotel_california (version)
import HotelCalifornia.Exec
import HotelCalifornia.Tracing (withGlobalTracing)
import Options.Applicative

data Command = Command
    { commandGlobalOptions :: GlobalOptions
    , commandSubCommand :: SubCommand
    }

data GlobalOptions = GlobalOptions

data SubCommand
    = Exec ExecArgs

optionsParser :: ParserInfo Command
optionsParser =
    info' parser' "Welcome to `hotel-california`"
  where
  -- thanks danidiaz for the blog post
    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info
        (helper <*> p)
        (fullDesc <> progDesc desc <> noIntersperse)

    parser' :: Parser Command
    parser' =
        Command
            <$> generalOptionsParser
            <*> subCommandParser
            <**> simpleVersioner (showVersion version)

    generalOptionsParser =
        pure GlobalOptions

    subCommandParser :: Parser SubCommand
    subCommandParser =
        subparser $ foldMap command'
                    [ ("exec", "Execute the given command with tracing enabled", Exec <$> parseExecArgs)
                    ]

    command' (cmdName,desc,parser) =
        command cmdName (info' parser desc)

main :: IO ()
main = do
    withGlobalTracing do
        let parserPrefs = defaultPrefs{ prefMultiSuffix = "..." }
        Command {..} <- customExecParser parserPrefs optionsParser
        case commandSubCommand of
            Exec execArgs ->
                runExecArgs execArgs
