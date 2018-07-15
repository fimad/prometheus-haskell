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
import qualified Data.Default as Default
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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
requestLatency :: Prom.Vector Prom.Label3 Prom.Histogram
requestLatency = Prom.unsafeRegister $ Prom.vector ("handler", "method", "status_code")
                                     $ Prom.histogram info Prom.defaultBuckets
    where info = Prom.Info "http_request_duration_seconds"
                           "The HTTP request latencies in seconds."

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp :: Text             -- ^ The label used to identify this app
              -> Wai.Application  -- ^ The app to instrument
              -> Wai.Application  -- ^ The instrumented app
instrumentApp handler app req respond = do
    start <- getTime Monotonic
    app req $ \res -> do
        end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res)))
        observeSeconds handler method status start end
        respond res

-- | Instrument an IO action with timing metrics. This function can be used if
-- you would like to get more fine grained metrics, for instance this can be
-- used to instrument individual end points.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentIO :: Text    -- ^ The label used to identify this IO operation
             -> IO a    -- ^ The IO action to instrument
             -> IO a    -- ^ The instrumented app
instrumentIO label io = do
    start  <- getTime Monotonic
    result <- io
    end    <- getTime Monotonic
    observeSeconds label Nothing Nothing start end
    return result

observeSeconds :: Text -> Maybe Text -> Maybe Text -> TimeSpec -> TimeSpec -> IO ()
observeSeconds handler method status start end = do
    let latency = fromRational $ toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
    Prom.withLabel requestLatency
                   (handler, fromMaybe "" method, fromMaybe "" status)
                   (flip Prom.observe latency)

-- | Expose Prometheus metrics and instrument an application with some basic
-- metrics (e.g. request latency).
prometheus :: PrometheusSettings -> Wai.Middleware
prometheus PrometheusSettings{..} app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == prometheusEndPoint
        -- XXX: Should probably be "metrics" rather than "prometheus", since
        -- "prometheus" can be confused with actual prometheus.
    then
      if prometheusInstrumentPrometheus
        then instrumentApp "prometheus" (const respondWithMetrics) req respond
        else respondWithMetrics respond
    else
      if prometheusInstrumentApp
        then instrumentApp "app" app req respond
        else app req respond


-- | WAI Application that serves the Prometheus metrics page regardless of
-- what the request is.
metricsApp :: Wai.Application
metricsApp = const respondWithMetrics

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
    metrics <- Prom.exportMetricsAsText
    respond $ Wai.responseLBS HTTP.status200 headers metrics
    where
        headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
