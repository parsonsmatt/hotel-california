-- | This module defines the code for actually executing a command with tracing
-- enabled.
module HotelCalifornia.Exec where

import Conduit
import Data.Conduit.Binary
import qualified Control.Exception as Exception
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import HotelCalifornia.Tracing
import HotelCalifornia.Tracing.TraceParent
import qualified OpenTelemetry.Trace.Core as Otel
import Options.Applicative hiding (command)
import System.Environment (getEnvironment)
import System.Exit
import qualified System.Posix.Escape.Unicode as Escape
import System.Process.Typed
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent.Async

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
    execArgsSubprocess <- parseSubprocess
    pure ExecArgs{..}

runExecArgs :: ExecArgs -> IO ()
runExecArgs ExecArgs {..} = do
    let script = commandToString execArgsSubprocess
        spanName =
            fromMaybe (Text.pack script) execArgsSpanName

    inSpan' spanName \span_ -> do
        newEnv <- spanContextToEnvironment span_
        fullEnv <- mappend newEnv <$> getEnvironment

        let processConfig = commandToProcessConfig execArgsSubprocess

        let handleSigInt =
                \case
                    Exception.UserInterrupt ->
                        case execArgsSigintStatus of
                            SpanUnset ->
                                pure Nothing
                            SpanOk -> do
                                Otel.setStatus span_ Otel.Ok
                                pure Nothing
                            SpanError -> do
                                Exception.throwIO Exception.UserInterrupt
                    other ->
                        Exception.throwIO other

        -- (mexitCode, stderr, stdout)
        mresult
            <- Exception.handle handleSigInt $ fmap Just $ runProcessCapturingOutput $ setEnv fullEnv processConfig

        case mresult of
            Just (ExitSuccess, _, _) ->
                pure ()
            Just (exitCode, stdoutLines, stderrLines) -> do
                print ("stdout: ", stdoutLines)
                print ("stderr: ", stderrLines)
                exitWith exitCode
            Nothing ->
                pure ()


runProcessCapturingOutput :: ProcessConfig stdin stdout stderr -> IO (ExitCode, BS.ByteString, BS.ByteString)
runProcessCapturingOutput config = do
    let finalproc = setStdout createPipe $ setStderr createPipe $ config
    withProcessWait finalproc \process -> do
        let procStderr = getStderr process
            procStdout = getStdout process
            teeHandle p o =
                runConduit do
                    sourceHandle p
                        .| conduitHandle o
                        .| takeLastNC 1024

        withAsync (teeHandle procStderr stderr) \stderrThread -> do
            withAsync (teeHandle procStdout stdout) \stdoutThread -> do
                exitCode <- waitExitCode process
                stderrContents <- wait stderrThread
                stdoutContents <- wait stdoutThread
                pure (exitCode, stderrContents, stdoutContents)

takeLastNC :: Monad m => Int -> ConduitT BS.ByteString o m BS.ByteString
takeLastNC i = go mempty
  where
    go !acc = do
        mbytes <- await
        case mbytes of
            Nothing ->
                pure acc
            Just bytes ->
                go (BS.takeEnd i (acc <> bytes))

