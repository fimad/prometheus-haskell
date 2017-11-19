{-# language GeneralizedNewtypeDeriving #-}
module Prometheus.Metric.Gauge (
    Gauge
,   gauge
,   incGauge
,   decGauge
,   addGauge
,   subGauge
,   setGauge
,   setGaugeToDuration
,   getGauge
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.Observer (timeAction)
import Prometheus.MonadMonitor

import Control.DeepSeq
import qualified Data.Atomics as Atomics
import qualified Data.ByteString.UTF8 as BS
import qualified Data.IORef as IORef


newtype Gauge = MkGauge (IORef.IORef Double)
  deriving (NFData)

-- | Create a new gauge metric with a given name and help string.
gauge :: Info -> IO (Metric Gauge)
gauge info = do
    ioref <- IORef.newIORef 0
    return Metric {
            handle = MkGauge ioref
        ,   collect = collectGauge info ioref
        }

withGauge :: MonadMonitor m
          => Metric Gauge
          -> (Double -> Double)
          -> m ()
withGauge (Metric {handle = MkGauge ioref}) f =
    doIO $ Atomics.atomicModifyIORefCAS_ ioref f

-- | Adds a value to a gauge metric.
addGauge :: MonadMonitor m => Double -> Metric Gauge -> m ()
addGauge x g = withGauge g add
    where add i = i `seq` x `seq` i + x

-- | Subtracts a value from a gauge metric.
subGauge :: MonadMonitor m => Double -> Metric Gauge -> m ()
subGauge x g = withGauge g sub
    where sub i = i `seq` x `seq` i - x

-- | Increments a gauge metric by 1.
incGauge :: MonadMonitor m => Metric Gauge -> m ()
incGauge g = withGauge g (+ 1)

-- | Decrements a gauge metric by 1.
decGauge :: MonadMonitor m => Metric Gauge -> m ()
decGauge g = withGauge g (+ (-1))

-- | Sets a gauge metric to a specific value.
setGauge :: MonadMonitor m => Double -> Metric Gauge -> m ()
setGauge r g = withGauge g set
    where set _ = r

-- | Retrieves the current value of a gauge metric.
getGauge :: Metric Gauge -> IO Double
getGauge (Metric {handle = MkGauge ioref}) = IORef.readIORef ioref

-- | Sets a gauge metric to the duration in seconds of an IO action.
setGaugeToDuration :: IO a -> Metric Gauge -> IO a
setGaugeToDuration io metric = do
    (result, duration) <- timeAction io
    setGauge duration metric
    return result

collectGauge :: Info -> IORef.IORef Double -> IO [SampleGroup]
collectGauge info c = do
    value <- IORef.readIORef c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info GaugeType [sample]]
