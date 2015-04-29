module Prometheus.Metric.Gauge (
    Gauge
,   gauge
,   incGauge
,   decGauge
,   addGauge
,   subGauge
,   setGauge
,   getGauge
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.MonadMonitor

import qualified Data.Atomics as Atomics
import qualified Data.ByteString.UTF8 as BS
import qualified Data.IORef as IORef


newtype Gauge = MkGauge (IORef.IORef Double)

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
addGauge x gauge = withGauge gauge add
    where add i = i `seq` x `seq` i + x

-- | Subtracts a value from a gauge metric.
subGauge :: MonadMonitor m => Double -> Metric Gauge -> m ()
subGauge x gauge = withGauge gauge sub
    where sub i = i `seq` x `seq` i - x

-- | Increments a gauge metric by 1.
incGauge :: MonadMonitor m => Metric Gauge -> m ()
incGauge gauge = withGauge gauge (+ 1)

-- | Decrements a gauge metric by 1.
decGauge :: MonadMonitor m => Metric Gauge -> m ()
decGauge gauge = withGauge gauge (+ (-1))

-- | Sets a gauge metric to a specific value.
setGauge :: MonadMonitor m => Double -> Metric Gauge -> m ()
setGauge r gauge = withGauge gauge set
    where set _ = r

-- | Retrieves the current value of a gauge metric.
getGauge :: Metric Gauge -> IO Double
getGauge (Metric {handle = MkGauge ioref}) = IORef.readIORef ioref

collectGauge :: Info -> IORef.IORef Double -> IO [SampleGroup]
collectGauge info c = do
    value <- IORef.readIORef c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info GaugeType [sample]]
