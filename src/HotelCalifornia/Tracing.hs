module HotelCalifornia.Tracing
    ( module HotelCalifornia.Tracing
    , defaultSpanArguments
    ) where

import Control.Monad
import Data.Char (toLower)
import Data.HashMap.Strict qualified as HashMap
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import HotelCalifornia.Tracing.TraceParent
import OpenTelemetry.Attributes (getAttributeMap)
import OpenTelemetry.Context as Context hiding (lookup)
import OpenTelemetry.Context.ThreadLocal (attachContext)
import OpenTelemetry.Propagator (setGlobalTextMapPropagator)
import OpenTelemetry.Resource
    ( MaterializedResources
    , getMaterializedResourcesAttributes
    , getMaterializedResourcesSchema
    , materializeResourcesWithSchema
    , mkResource
    , (.=)
    )
import OpenTelemetry.Trace hiding
    ( SpanKind (..)
    , SpanStatus (..)
    , addAttribute
    , addAttributes
    , createSpan
    , inSpan
    , inSpan'
    , inSpan''
    )
import OpenTelemetry.Trace qualified as Trace
import System.Environment (getEnvironment)
import UnliftIO

-- | Initialize the global tracing provider for the application and run an action
--   (that action is generally the entry point of the application), cleaning
--   up the provider afterwards.
--
--   This also sets up an empty context (creating a new trace ID).
--
--   The first argument optionally overrides the service name that spans are
--   reported under, taking precedence over @OTEL_SERVICE_NAME@. Because it is
--   applied to the tracer provider's resources rather than to the environment,
--   it does not affect the service name reported by any subprocess.
--
--   The callback receives a 'TracingStatus' describing whether tracing was
--   actually initialized; see 'tracingEnabled'.
withGlobalTracing
    :: (MonadUnliftIO m) => Maybe Text -> (TracingStatus -> m a) -> m a
withGlobalTracing mServiceName act = do
    void $ attachContext Context.empty
    liftIO setParentSpanFromEnvironment
    enabled <- liftIO otelTracingEnabled
    if enabled
        then withTracer mServiceName \_ -> act TracingStatus{tracingEnabled = True}
        else act TracingStatus{tracingEnabled = False}

-- | The result of setting up tracing, passed to the callback of
--   'withGlobalTracing'.
data TracingStatus = TracingStatus
    { tracingEnabled :: Bool
    -- ^ 'True' when an exporter is configured (see 'otelTracingEnabled') and
    --   tracing has been initialized, and 'False' otherwise -- in which case
    --   the caller should bypass tracing.
    }

-- | Decide whether tracing should be initialized, following the OpenTelemetry
--   [environment variable specification](https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/)
--   where practical:
--
--   * @OTEL_SDK_DISABLED=true@ (case-insensitive) disables tracing.
--   * @OTEL_TRACES_EXPORTER=none@ disables tracing; any other non-empty value
--     enables it.
--   * Otherwise, tracing is enabled iff any @OTEL_EXPORTER_*@ environment
--     variable is set with a non-empty value.
--
--   The spec would have tracing enabled unconditionally, with
--   @OTEL_TRACES_EXPORTER@ defaulting to an OTLP exporter aimed at
--   @localhost@; since running with no collector at all is the common case
--   for a CLI tool, we deviate and treat exporter configuration as opt-in.
--
--   Per the spec, an environment variable set to the empty string is treated
--   the same as unset.
otelTracingEnabled :: IO Bool
otelTracingEnabled = do
    env <- getEnvironment
    let
        getVar key = do
            value <- lookup key env
            guard $ not $ null value
            pure $ map toLower value
        sdkDisabled = getVar "OTEL_SDK_DISABLED" == Just "true"
        tracesExporter = getVar "OTEL_TRACES_EXPORTER"
        hasOtelExporterVar = any isOtelExporterVar env
    pure $
        not sdkDisabled
            && tracesExporter /= Just "none"
            && (isJust tracesExporter || hasOtelExporterVar)
  where
    isOtelExporterVar (k, v) = "OTEL_EXPORTER_" `isPrefixOf` k && not (null v)

globalTracer :: (MonadIO m) => m Tracer
globalTracer =
    getGlobalTracerProvider >>= \tp -> pure $ makeTracer tp "hotel-california" tracerOptions

inSpan' :: (MonadUnliftIO m) => Text -> (Span -> m a) -> m a
inSpan' spanName =
    inSpanWith' spanName defaultSpanArguments

inSpanWith :: (MonadUnliftIO m) => Text -> SpanArguments -> m a -> m a
inSpanWith spanName args action =
    inSpanWith' spanName args \_ -> action

inSpanWith'
    :: (MonadUnliftIO m) => Text -> SpanArguments -> (Span -> m a) -> m a
inSpanWith' spanName args action = do
    tr <- globalTracer
    Trace.inSpan'' tr spanName args action

inSpan :: (MonadUnliftIO m) => Text -> m a -> m a
inSpan spanName =
    inSpanWith spanName defaultSpanArguments
withTracer :: (MonadUnliftIO m) => Maybe Text -> (TracerProvider -> m a) -> m a
withTracer mServiceName =
    bracket
        (liftIO $ initializeGlobalTracerProviderWithServiceName mServiceName)
        \tp -> shutdownTracerProvider tp Nothing

-- | Like 'initializeGlobalTracerProvider', but with the service name that
--   resources are reported with optionally overridden.
--
--   Note that the override cannot be applied by setting @OTEL_SERVICE_NAME@:
--   the SDK spawns subprocesses of its own while detecting resources (@ioreg@
--   and @sw_vers@ on macOS, for example), and a modified environment would be
--   inherited by them and by anything else the program runs later.
--
--   Note that this reimplements 'initializeGlobalTracerProvider' rather than
--   delegating to it when there is no override, so that passing @--service@ and
--   omitting it cannot diverge if the SDK grows an extra initialization step.
initializeGlobalTracerProviderWithServiceName :: Maybe Text -> IO TracerProvider
initializeGlobalTracerProviderWithServiceName mServiceName = do
    (processors, options) <- getTracerProviderInitializationOptions
    -- Install the propagators that the SDK detected from the environment,
    -- which 'Trace.initializeTracerProvider' would have done for us if we
    -- could have used it.
    setGlobalTextMapPropagator $ tracerProviderOptionsPropagators options
    tracerProvider <-
        createTracerProvider processors $
            options
                { tracerProviderOptionsResources =
                    maybe id overrideServiceName mServiceName $
                        tracerProviderOptionsResources options
                }
    setGlobalTracerProvider tracerProvider
    pure tracerProvider

-- | Replace the @service.name@ attribute of some already-detected resources.
--
--   The SDK resolves the service name when the tracer provider is initialized,
--   and gives @OTEL_SERVICE_NAME@ precedence over any resource passed to
--   'getTracerProviderInitializationOptions'', so overriding the service name
--   means editing the resources that it produced.
overrideServiceName :: Text -> MaterializedResources -> MaterializedResources
overrideServiceName serviceName resources =
    -- The schema URL has to be carried over explicitly, since 'mkResource'
    -- does not preserve it.
    materializeResourcesWithSchema (getMaterializedResourcesSchema resources) $
        mkResource $
            (serviceNameKey .= serviceName)
                : map Just (HashMap.toList otherAttributes)
  where
    otherAttributes =
        HashMap.delete serviceNameKey $
            getAttributeMap $
                getMaterializedResourcesAttributes resources

-- | The resource attribute that the service name is reported as.
serviceNameKey :: Text
serviceNameKey = "service.name"
