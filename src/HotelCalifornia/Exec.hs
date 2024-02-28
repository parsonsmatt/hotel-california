-- | This module defines the code for actually executing a command with tracing
-- enabled.
module HotelCalifornia.Exec where

import qualified Control.Exception as Exception
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text)
import HotelCalifornia.Tracing
import HotelCalifornia.Tracing.TraceParent
import qualified OpenTelemetry.Trace.Core as Otel
import OpenTelemetry.Trace (Attribute(..), PrimitiveAttribute(..))
import Options.Applicative hiding (command)
import System.Environment (getEnvironment)
import System.Exit
import qualified System.Posix.Escape.Unicode as Escape
import System.Process.Typed
import HotelCalifornia.Which (which)

data Subprocess = Proc (NonEmpty String) | Shell String

commandToString :: Subprocess -> String
commandToString (Proc tokens) = Escape.escapeMany (NEL.toList tokens)
commandToString (Shell line) = line

commandToProcessConfig :: Subprocess -> ProcessConfig () () ()
commandToProcessConfig (Proc (command :| args)) = proc command args
commandToProcessConfig (Shell line) = shell line

data ExecArgs = ExecArgs
    { execArgsSubprocess :: Subprocess
    , execArgsSpanName :: Maybe Text
    , execArgsSigintStatus :: SpanStatus'
    , execArgsAttributes :: HashMap Text Attribute
    }

-- | A variant of 'SpanStatus' that does not include a 'Text' for error.
data SpanStatus'
    = SpanUnset
    | SpanOk
    | SpanError

parseSpanStatus' :: ReadM SpanStatus'
parseSpanStatus' = eitherReader \s ->
    case map Char.toLower s of
        "unset" -> Right SpanUnset
        "ok" -> Right SpanOk
        "error" -> Right SpanError
        _ -> Left $ mconcat ["Expected one of `unset`, `ok`, or `error` for SPAN_STATUS. Got: ", s]

parseProc :: Parser (NonEmpty String)
parseProc = do
    command <- argument str (metavar "COMMAND")
    arguments <- many (argument str (metavar "ARGUMENT"))
    return (command :| arguments)

parseShell :: Parser String
parseShell =
    option str
        (   metavar "SCRIPT"
        <>  long "shell"
        <>  help "Run an arbitrary shell script instead of running an executable command"
        )

parseSubprocess :: Parser Subprocess
parseSubprocess = fmap Proc parseProc <|> fmap Shell parseShell

-- | Parse a `key=value` string into an attribute.
parseAttribute :: String -> Either String (Text, Attribute)
parseAttribute input = do
    let (key, value') = Text.breakOn "=" $ Text.pack input
    if Text.null value' || Text.null key
    then Left $ "Attributes must contain a non-empty key and value separated by `=`: " <> input
    else pure $ (key, AttributeValue $ TextAttribute $ Text.drop 1 value')

parseExecArgs :: Parser ExecArgs
parseExecArgs = do
    execArgsSpanName <- optional do
        option str $ mconcat
            [ metavar "SPAN_NAME"
            , long "span-name"
            , short 's'
            , help "The name of the span that the program reports. By default, this is the script you pass in."
            ]
    execArgsSigintStatus <-
        option parseSpanStatus' $ mconcat
            [ metavar "SPAN_STATUS"
            , long "set-sigint-status"
            , short 'i'
            , help "The status reported when the process is killed with SIGINT."
            , value SpanUnset
            ]
    execArgsAttributes <-
        HashMap.fromList <$> many (option (eitherReader parseAttribute) $ mconcat
            [ metavar "KEY=VALUE"
            , long "attribute"
            , short 'a'
            , help "A string attribute to add to the span."
            ])
    execArgsSubprocess <- parseSubprocess
    pure ExecArgs{..}

makeInitialAttributes :: Subprocess -> HashMap Text Attribute -> IO (HashMap Text Attribute)
makeInitialAttributes subprocess extraAttributes = do
    processAttributes <-
        case subprocess of
          Proc (command :| args) -> do
              pathAttribute <-
                  (foldMap (\path -> [(executablePathName, Otel.toAttribute $ Text.pack path)]))
                  <$> which command
              pure $ HashMap.fromList $
                [ (commandArgsName, Otel.toAttribute $ map Text.pack args)
                , (executableName, Otel.toAttribute $ Text.pack command)
                ] <> pathAttribute
          Shell _command -> pure mempty

    pure $ processAttributes <> extraAttributes

runExecArgs :: ExecArgs -> IO ()
runExecArgs ExecArgs {..} = do
    initialAttributes <- makeInitialAttributes execArgsSubprocess execArgsAttributes

    let script = commandToString execArgsSubprocess
        spanName =
            fromMaybe (Text.pack script) execArgsSpanName
        spanArguments = defaultSpanArguments { Otel.attributes = initialAttributes }

    inSpanWith' spanName spanArguments \span' -> do
        newEnv <- spanContextToEnvironment span'
        fullEnv <- mappend newEnv <$> getEnvironment

        let processConfig = commandToProcessConfig execArgsSubprocess

        let handleSigInt =
                \case
                    Exception.UserInterrupt -> do
                        Otel.addAttribute span' exitStatusName (-2 :: Int) -- SIGINT
                        case execArgsSigintStatus of
                            SpanUnset ->
                                pure Nothing
                            SpanOk -> do
                                Otel.setStatus span' Otel.Ok
                                pure Nothing
                            SpanError -> do
                                -- `hs-opentelemetry` will automatically mark a
                                -- span as an error if it ends with an
                                -- exception.
                                Exception.throwIO Exception.UserInterrupt
                    other ->
                        Exception.throwIO other

        mexitCode <- Exception.handle handleSigInt $ fmap Just $ runProcess $ setEnv fullEnv processConfig

        case mexitCode of
            Just exitCode -> do
                Otel.addAttribute span' exitStatusName
                    case exitCode of
                       ExitSuccess -> 0
                       ExitFailure status -> status
                case exitCode of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> exitWith exitCode
            Nothing ->
                pure ()

exitStatusName :: Text
exitStatusName = "process.exit_status"

executablePathName :: Text
executablePathName = "process.executable.path"

executableName :: Text
executableName = "process.executable.path"

commandArgsName :: Text
commandArgsName = "process.command_args"
