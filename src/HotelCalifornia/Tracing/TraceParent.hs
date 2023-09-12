-- | This module defines the type of 'TraceParent' which can be parsed
module HotelCalifornia.Tracing.TraceParent where

import Data.Foldable (for_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as Text
import OpenTelemetry.Propagator.W3CTraceContext
import OpenTelemetry.Trace.Core (SpanContext, isRemote, wrapSpanContext, Span)
import System.Environment
import OpenTelemetry.Context.ThreadLocal
import qualified OpenTelemetry.Context as Ctxt

-- | This function looks up the @TRACEPARENT@ and @TRACECONTEXT@ environment
-- variables and returns a @'Maybe' 'SpanContext'@ constructed from them.
spanContextFromEnvironment :: IO (Maybe SpanContext)
spanContextFromEnvironment = do
    mtraceParent <- lookupEnvBS "TRACEPARENT"
    mtraceContext <- lookupEnvBS "TRACESTATE"
    pure $ decodeSpanContext mtraceParent mtraceContext
  where
    lookupEnvBS :: String -> IO (Maybe BS.ByteString)
    lookupEnvBS str = fmap (TE.encodeUtf8 . Text.pack) <$> lookupEnv str

-- | This function takes the given 'Span' and converts it into a list of
-- environment variables consisting of:
--
-- @
-- [ ( "TRACEPARENT", traceParent)
-- , ( "TRACESTATE", traceState)
-- ]
-- @
spanContextToEnvironment :: Span -> IO [(String, String)]
spanContextToEnvironment spanContext = do
    (traceParent, traceState) <- encodeSpanContext spanContext
    pure
        [ ("TRACEPARENT", BS8.unpack traceParent)
        , ("TRACESTATE", BS8.unpack traceState)
        ]


-- | This function should be called after you've initialized and attached the
-- thread local 'Context'.
setParentSpanFromEnvironment :: IO ()
setParentSpanFromEnvironment = do
    mspanContext <- spanContextFromEnvironment
    for_ mspanContext \spanContext -> do
        adjustContext $ Ctxt.insertSpan (wrapSpanContext (spanContext {isRemote = True}))
