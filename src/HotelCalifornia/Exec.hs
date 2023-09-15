-- | This module defines the code for actually executing a command with tracing
-- enabled.
module HotelCalifornia.Exec where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import HotelCalifornia.Tracing
import HotelCalifornia.Tracing.TraceParent
import System.Environment (getEnvironment)
import qualified System.Posix.Escape.Unicode as Escape
import Options.Applicative hiding (command)
import System.Exit
import System.Process.Typed

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
    }

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

        exitCode <- runProcess $ setEnv fullEnv processConfig
        case exitCode of
            ExitSuccess ->
                pure ()
            _ ->
                exitWith exitCode
