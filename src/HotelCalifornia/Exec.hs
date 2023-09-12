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
import Options.Applicative
import System.Exit
import System.Process.Typed

data ExecArgs = ExecArgs
    { execArgsScript :: NonEmpty String
    , execArgsSpanName :: Maybe Text
    }

parseExecArgs :: Parser ExecArgs
parseExecArgs = do
    execArgsSpanName <- optional do
        option str $ mconcat
            [ metavar "SPAN_NAME"
            , long "span-name"
            , short 's'
            , help "The name of the span that the program reports. By default, this is the script you pass in."
            ]
    execArgsScript1 <- argument str (metavar "SCRIPT" <> help "The command to run, along with any arguments. Best to use -- before providing the script, otherwise it may pass arguments to `hotel` instead of to your script")
    execArgsScriptRest <- many $ argument str (metavar "SCRIPT...")
    pure ExecArgs
        { execArgsScript = execArgsScript1 :| execArgsScriptRest
        , ..
        }

runExecArgs :: ExecArgs -> IO ()
runExecArgs ExecArgs {..} = do
    let script =
            unwords $ NEL.toList execArgsScript
        spanName =
            fromMaybe (Text.pack script) execArgsSpanName

    inSpan' spanName \span_ -> do
        newEnv <- spanContextToEnvironment span_
        fullEnv <- mappend newEnv <$> getEnvironment

        let processConfig = shell $ unwords $ NEL.toList execArgsScript
        exitCode <- runProcess $ setEnv fullEnv $ processConfig
        case exitCode of
            ExitSuccess ->
                pure ()
            _ ->
                exitWith exitCode
