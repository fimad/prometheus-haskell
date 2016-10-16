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

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.ByteString.Builder as BS
import qualified Data.Default as Default
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom


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
requestLatency :: Prom.Metric (Prom.Vector Prom.Label1 Prom.Summary)
requestLatency = Prom.unsafeRegisterIO $ Prom.vector "handler"
                                       $ Prom.summary info Prom.defaultQuantiles
    where info = Prom.Info "http_request_duration_microseconds"
                           "The HTTP request latencies in microseconds."

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp :: String           -- ^ The label used to identify this app
              -> Wai.Application  -- ^ The app to instrument
              -> Wai.Application  -- ^ The instrumented app
instrumentApp handler app req respond =
    observeMicroSeconds handler (app req respond)

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
instrumentIO = observeMicroSeconds

observeMicroSeconds :: String -> IO a -> IO a
observeMicroSeconds handler io = do
    start  <- getCurrentTime
    result <- io
    end    <- getCurrentTime
    let latency = fromRational $ toRational (end `diffUTCTime` start) * 1000000
    Prom.withLabel handler (Prom.observe latency) requestLatency
    return result

-- | Expose Prometheus metrics and instrument an application with some basic
-- metrics (e.g. request latency).
prometheus :: PrometheusSettings -> Wai.Middleware
prometheus PrometheusSettings{..} app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == prometheusEndPoint
    then measure measureMetrics "prometheus" $ respondWithMetrics respond
    else measure measureApp "app" $ app req respond
    where
        measureMetrics = prometheusInstrumentPrometheus
        measureApp = prometheusInstrumentApp
        measure shouldInstrument handler io
            | shouldInstrument = observeMicroSeconds handler io
            | otherwise        = io


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
