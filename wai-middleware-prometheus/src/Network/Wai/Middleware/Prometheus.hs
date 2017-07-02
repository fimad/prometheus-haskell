-- | This module provides "Network.Wai" middlware for exporting "Prometheus"
-- metrics and for instrumenting WAI applications.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Middleware.Prometheus (
    prometheus
,   PrometheusSettings (..)
,   Default.def
,   instrumentApp
,   instrumentIO
,   metricsApp
) where

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Default as Default
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom
import System.Clock (Clock(..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)


-- | Settings that control the behavior of the Prometheus middleware.
data PrometheusSettings = PrometheusSettings {
        prometheusEndPoint             :: [T.Text]
        -- ^ The path that will be used for exporting metrics. The default value
        -- is ["metrics"] which corresponds to the path /metrics.
    ,   prometheusInstrumentApp        :: Bool
        -- ^ Whether the default instrumentation should be applied to the
        -- application. If this is set to false the application can still be
        -- instrumented using the 'instrumentApp' function. The default value is
        -- True.
    ,   prometheusInstrumentPrometheus :: Bool
        -- ^ Whether the default instrumentation should be applied to the
        -- middleware that serves the metrics endpoint. The default value is
        -- True.
    }

instance Default.Default PrometheusSettings where
    def = PrometheusSettings {
        prometheusEndPoint             = ["metrics"]
    ,   prometheusInstrumentApp        = True
    ,   prometheusInstrumentPrometheus = True
    }

{-# NOINLINE requestLatency #-}
-- XXX: https://prometheus.io/docs/practices/naming/ says this should be
-- _seconds, not _microseconds.
requestLatency :: Prom.Metric (Prom.Vector Prom.Label3 Prom.Summary)
requestLatency = Prom.unsafeRegisterIO $ Prom.vector ("handler", "method", "status_code")
                                       $ Prom.summary info Prom.defaultQuantiles
    where info = Prom.Info "http_request_duration_seconds"
                           "The HTTP request latencies in seconds."

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp :: String           -- ^ The label used to identify this app
              -> Wai.Application  -- ^ The app to instrument
              -> Wai.Application  -- ^ The instrumented app
instrumentApp handler app req respond = do
    start <- getTime Monotonic
    app req $ \res -> do
        end <- getTime Monotonic
        let method = Just $ BS.unpack (Wai.requestMethod req)
        let status = Just $ show (HTTP.statusCode (Wai.responseStatus res))
        observeSeconds handler method status start end
        respond res

-- | Instrument an IO action with timing metrics. This function can be used if
-- you would like to get more fine grained metrics, for instance this can be
-- used to instrument individual end points.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentIO :: String  -- ^ The label used to identify this IO operation
             -> IO a    -- ^ The IO action to instrument
             -> IO a    -- ^ The instrumented app
instrumentIO label io = do
    start  <- getTime Monotonic
    result <- io
    end    <- getTime Monotonic
    observeSeconds label Nothing Nothing start end
    return result

observeSeconds :: String -> Maybe String -> Maybe String -> TimeSpec -> TimeSpec -> IO ()
observeSeconds handler method status start end = do
    let latency = fromRational $ toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
    Prom.withLabel (handler, fromMaybe "" method, fromMaybe "" status)
                   (Prom.observe latency)
                   requestLatency

-- | Expose Prometheus metrics and instrument an application with some basic
-- metrics (e.g. request latency).
prometheus :: PrometheusSettings -> Wai.Middleware
prometheus PrometheusSettings{..} app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == prometheusEndPoint
        -- XXX: Should probably be "metrics" rather than "prometheus", since
        -- "prometheus" can be confused with actual prometheus.
    then instrumentApp "prometheus" (const respondWithMetrics) req respond
    else instrumentApp "app" app req respond


-- | WAI Application that serves the Prometheus metrics page regardless of
-- what the request is.
metricsApp :: Wai.Application
metricsApp = const respondWithMetrics

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
    metrics <- Prom.exportMetricsAsText
    respond $ Wai.responseBuilder HTTP.status200 headers $ BS.byteString metrics
    where
        headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
