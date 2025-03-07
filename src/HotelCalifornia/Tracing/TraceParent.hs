-- | This module defines the type of 'TraceParent' which can be parsed
module HotelCalifornia.Tracing.TraceParent
    ( spanContextFromEnvironment
    , baggageFromEnvironment
    , spanContextToEnvironment
    , setParentSpanFromEnvironment
    ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import OpenTelemetry.Baggage (Baggage)
import OpenTelemetry.Context qualified as Ctxt
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Propagator.W3CBaggage qualified as W3CBaggage
import OpenTelemetry.Propagator.W3CTraceContext
import OpenTelemetry.Trace.Core (Span, SpanContext, isRemote, wrapSpanContext)
import System.Environment

-- | This function looks up the @TRACEPARENT@ and @TRACECONTEXT@ environment
-- variables and returns a @'Maybe' 'SpanContext'@ constructed from them.
spanContextFromEnvironment :: IO (Maybe SpanContext)
spanContextFromEnvironment = do
    mtraceParent <- lookupEnvBS "TRACEPARENT"
    mtraceContext <- lookupEnvBS "TRACESTATE"
    pure $ decodeSpanContext mtraceParent mtraceContext

-- | This function looks up the @BAGGAGE@ environment variable and returns a
-- @'Maybe' 'Baggage'@ constructed from that.
baggageFromEnvironment :: IO (Maybe Baggage)
baggageFromEnvironment = do
    mBaggageBytes <- lookupEnvBS "BAGGAGE"

    let
        mBaggage = do
            baggageBytes <- mBaggageBytes
            W3CBaggage.decodeBaggage baggageBytes

    pure mBaggage

lookupEnvBS :: String -> IO (Maybe BS.ByteString)
lookupEnvBS str = fmap (TE.encodeUtf8 . Text.pack) <$> lookupEnv str

-- | This function takes the given 'Span' and converts it into a list of
-- environment variables consisting of:
--
-- @
-- [ ( "TRACEPARENT", traceParent)
-- , ( "TRACESTATE", traceState)
-- , ( "BAGGAGE", traceParent)
-- ]
-- @
spanContextToEnvironment :: Span -> IO [(String, String)]
spanContextToEnvironment span_ = do
    (traceParent, traceState) <- encodeSpanContext span_

    context <- getContext

    let
        baggageVariables =
            case Ctxt.lookupBaggage context of
                Just baggage ->
                    [("BAGGAGE", BS8.unpack (W3CBaggage.encodeBaggage baggage))]
                Nothing ->
                    []

    pure
        ( [ ("TRACEPARENT", BS8.unpack traceParent)
          , ("TRACESTATE", BS8.unpack traceState)
          ]
            <> baggageVariables
        )

-- | This function should be called after you've initialized and attached the
-- thread local 'Context'.
setParentSpanFromEnvironment :: IO ()
setParentSpanFromEnvironment = do
    mSpanContext <- spanContextFromEnvironment
    mBaggage <- baggageFromEnvironment

    let
        insertSpanContext =
            case mSpanContext of
                Nothing ->
                    id
                Just spanContext ->
                    Ctxt.insertSpan (wrapSpanContext spanContext{isRemote = True})

    let
        insertBaggage =
            case mBaggage of
                Nothing -> id
                Just baggage -> Ctxt.insertBaggage baggage

    adjustContext (insertBaggage . insertSpanContext)
