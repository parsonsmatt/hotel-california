{-# language ApplicativeDo #-}

module Main where

import Data.ByteString.Char8 qualified as BS8
import OpenTelemetry.Vendor.Honeycomb qualified as Honeycomb
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Options.Applicative
import System.Process.Typed
import UnliftIO
import OpenTelemetry.Context as Context hiding (lookup)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal (adjustContext, attachContext, getContext)
import OpenTelemetry.Trace hiding
  ( SpanKind (..),
    SpanStatus (..),
    addAttribute,
    addAttributes,
    createSpan,
    inSpan,
    inSpan',
    inSpan'',
  )
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Sampler
import OpenTelemetry.Utils.Exceptions qualified as TraceM
import OpenTelemetry.Vendor.Honeycomb qualified as Honeycomb
import Control.Monad
import Data.Time

data Command = Command
    { commandGlobalOptions :: GlobalOptions
    , commandSubCommand :: SubCommand
    }

data GlobalOptions = GlobalOptions

data SubCommand
    = Exec ExecArgs

data ExecArgs = ExecArgs
    { execArgsScript :: NonEmpty String
    , execArgsSpanName :: Maybe Text
    }

optionsParser :: ParserInfo Command
optionsParser =
    info' parser' "Welcome to `hotel-california`"
  where
  -- thanks danidiaz for the blog post
    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info
        (helper <*> p)
        (fullDesc <> progDesc desc)

    parser' :: Parser Command
    parser' =
        Command
            <$> generalOptionsParser
            <*> subCommandParser

    generalOptionsParser =
        pure GlobalOptions

    subCommandParser :: Parser SubCommand
    subCommandParser =
        subparser $ foldMap command'
                    [ ("exec", "Execute the given command with tracing enabled", Exec <$> execParser)
                    ]

    command' (cmdName,desc,parser) =
        command cmdName (info' parser desc)

    execParser :: Parser ExecArgs
    execParser = do
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

main :: IO ()
main = do
    withGlobalTracing do
        Command {..} <- execParser optionsParser
        case commandSubCommand of
            Exec ExecArgs {..} -> do
                let script =
                        unwords $ NEL.toList execArgsScript
                    spanName =
                        fromMaybe (Text.pack script) execArgsSpanName

                print ("args: ", execArgsScript)
                tp <- globalTracer
                exitCode <-
                    inSpanIO spanName defaultSpanArguments do
                        runProcess $ shell $ unwords $ NEL.toList execArgsScript
                print ("exit code: ", exitCode)

globalTracer :: MonadIO m => m Tracer
globalTracer = getGlobalTracerProvider >>= \tp -> getTracer tp "hotel-california" tracerOptions

inSpanIO :: (MonadUnliftIO m, HasCallStack) => Text -> SpanArguments -> m a -> m a
inSpanIO t a m = globalTracer >>= \tr -> Trace.inSpan tr t a m

-- | Initialize the global tracing provider for the application and run an action
--   (that action is generally the entry point of the application), cleaning
--   up the provider afterwards.
--
--   This also sets up an empty context (creating a new trace ID).
withGlobalTracing :: MonadUnliftIO m => m a -> m a
withGlobalTracing act = do
    void $ attachContext Context.empty
    bracket initializeTracing shutdownTracerProvider $ \_ -> do
        -- note: this is not in a span since we don't have a root span yet so it
        -- would not wind up in the trace in a helpful way anyway
        void $
          Honeycomb.getOrInitializeHoneycombTargetInContext initializationTimeout
            `catch` \(e :: SomeException) -> do
              -- we are too early in initialization to be able to use a normal logger,
              -- but this needs to get out somehow.
              --
              -- honeycomb links are not load-bearing, so we let them just not come
              -- up if the API fails.
              liftIO . BS8.hPutStrLn stderr $ "error setting up Honeycomb trace links: " <> (BS8.pack $ displayException e)
              pure Nothing

        act
  where
    initializationTimeout = secondsToNominalDiffTime 3

initializeTracing :: MonadUnliftIO m => m TracerProvider
initializeTracing = do
  (processors, tracerOptions') <- liftIO getTracerProviderInitializationOptions
  provider <- createTracerProvider processors tracerOptions'
  setGlobalTracerProvider provider
  pure provider

