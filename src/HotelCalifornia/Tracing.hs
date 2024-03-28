module HotelCalifornia.Tracing
    ( module HotelCalifornia.Tracing
    , defaultSpanArguments
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Time
import HotelCalifornia.Tracing.TraceParent
import OpenTelemetry.Context as Context hiding (lookup)
import OpenTelemetry.Context.ThreadLocal (attachContext)
import OpenTelemetry.Trace hiding
       ( SpanKind(..)
       , SpanStatus(..)
       , addAttribute
       , addAttributes
       , createSpan
       , inSpan
       , inSpan'
       , inSpan''
       )
import qualified OpenTelemetry.Trace as Trace
import qualified OpenTelemetry.Vendor.Honeycomb as Honeycomb
import UnliftIO

-- | Initialize the global tracing provider for the application and run an action
--   (that action is generally the entry point of the application), cleaning
--   up the provider afterwards.
--
--   This also sets up an empty context (creating a new trace ID).
withGlobalTracing :: MonadUnliftIO m => (Maybe Honeycomb.HoneycombTarget -> m a) -> m a
withGlobalTracing act = do
    void $ attachContext Context.empty
    liftIO setParentSpanFromEnvironment
    bracket (liftIO initializeGlobalTracerProvider) shutdownTracerProvider $ \_ -> do
        -- note: this is not in a span since we don't have a root span yet so it
        -- would not wind up in the trace in a helpful way anyway
        mTarget <-
          Honeycomb.getOrInitializeHoneycombTargetInContext initializationTimeout
            `catch` \(e :: SomeException) -> do
              -- we are too early in initialization to be able to use a normal logger,
              -- but this needs to get out somehow.
              --
              -- honeycomb links are not load-bearing, so we let them just not come
              -- up if the API fails.
              liftIO . BS8.hPutStrLn stderr $ "error setting up Honeycomb trace links: " <> (BS8.pack $ displayException e)
              pure Nothing

        act mTarget
  where
    initializationTimeout = secondsToNominalDiffTime 1

globalTracer :: MonadIO m => m Tracer
globalTracer = getGlobalTracerProvider >>= \tp -> pure $ makeTracer tp "hotel-california" tracerOptions

inSpan' :: (MonadUnliftIO m) => Text -> (Span -> m a) -> m a
inSpan' spanName =
    inSpanWith' spanName defaultSpanArguments

inSpanWith :: (MonadUnliftIO m) => Text -> SpanArguments -> m a -> m a
inSpanWith spanName args action =
    inSpanWith' spanName args \_ -> action

inSpanWith' :: (MonadUnliftIO m) => Text -> SpanArguments -> (Span -> m a) -> m a
inSpanWith' spanName args action = do
    tr <- globalTracer
    Trace.inSpan'' tr spanName args action

inSpan :: (MonadUnliftIO m) => Text -> m a -> m a
inSpan spanName =
    inSpanWith spanName defaultSpanArguments
