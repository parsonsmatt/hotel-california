module Main where

import Data.Version (showVersion)
import Paths_hotel_california (version)
import HotelCalifornia.Exec
import HotelCalifornia.Tracing (withGlobalTracing)
import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, vsep)

data Command = Command
    { commandGlobalOptions :: GlobalOptions
    , commandSubCommand :: SubCommand
    }

data GlobalOptions = GlobalOptions

data SubCommand
    = Exec ExecArgs

programDescription :: Doc
programDescription = vsep
  [ "`hotel-california` is a tool for OTel tracing of shell scripts, inspired by `otel-cli`."
  , "For help with a command, say `hotel COMMAND --help`. Currently, the only supported command is `exec`."
  , ""
  , "Check out the repository any time you like at https://github.com/parsonsmatt/hotel-california."
  ]

optionsParser :: ParserInfo Command
optionsParser =
    info' parser' programDescription
  where
  -- thanks danidiaz for the blog post
    info' :: Parser a -> Doc -> ParserInfo a
    info' p descDoc = info
        (helper <*> p)
        (fullDesc <> progDescDoc (Just descDoc) <> noIntersperse)

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
    withGlobalTracing $ \mTarget -> do
        let parserPrefs = defaultPrefs{ prefMultiSuffix = "..." }
        Command {..} <- customExecParser parserPrefs optionsParser
        case commandSubCommand of
            Exec execArgs ->
                case mTarget of
                    Just _target -> runExecArgs execArgs
                    Nothing -> runNoTracing $ execArgsSubprocess execArgs
