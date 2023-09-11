-- | This module defines the code for actually executing a command with tracing
-- enabled.
module HotelCalifornia.Exec where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import HotelCalifornia.Tracing
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
            ]
    execArgsScript1 <- argument str (metavar "SCRIPT")
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

    -- print ("args: ", execArgsScript)
    exitCode <-
        inSpan spanName do
            runProcess $ shell $ unwords $ NEL.toList execArgsScript
    -- print ("exit code: ", exitCode)
    exitWith exitCode
