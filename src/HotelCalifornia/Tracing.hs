module HotelCalifornia.Tracing
    ( module HotelCalifornia.Tracing
    , defaultSpanArguments
    ) where

import Control.Monad
import Data.List (isPrefixOf)
import Data.Text (Text)
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
import System.Environment (getEnvironment)
import UnliftIO

-- | Initialize the global tracing provider for the application and run an action
--   (that action is generally the entry point of the application), cleaning
--   up the provider afterwards.
--
--   This also sets up an empty context (creating a new trace ID).
--
--   The callback receives 'True' when an OTLP exporter is configured (any
--   @OTEL_EXPORTER_*@ env-var is set) and tracing has been initialized, and
--   'False' otherwise — in which case the caller should bypass tracing.
withGlobalTracing :: MonadUnliftIO m => (Bool -> m a) -> m a
withGlobalTracing act = do
    void $ attachContext Context.empty
    liftIO setParentSpanFromEnvironment
    hasOtelExporter <- liftIO otelExporterConfigured
    if hasOtelExporter
        then bracket (liftIO initializeGlobalTracerProvider) shutdownTracerProvider $ \_ ->
            act True
        else act False

-- | Returns 'True' iff any @OTEL_EXPORTER_*@ environment variable is set with
--   a non-empty value. Used to decide whether to initialize tracing at all.
otelExporterConfigured :: IO Bool
otelExporterConfigured =
    any isOtelExporter <$> getEnvironment
  where
    isOtelExporter (k, v) = "OTEL_EXPORTER_" `isPrefixOf` k && not (null v)

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
