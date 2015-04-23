module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
,   getCounter
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.MonadMonitor

import Data.Atomics.Counter as AtomicCounter
import qualified Data.ByteString.UTF8 as BS


newtype Counter = MkCounter AtomicCounter.AtomicCounter

-- | Creates a new counter metric with a given name and help string.
counter :: Info -> IO (Metric Counter)
counter info = do
    atomicCounter <- AtomicCounter.newCounter 0
    return Metric {
            handle = MkCounter atomicCounter
        ,   collect = collectCounter info atomicCounter
        }

-- | Increments the value of a counter metric by 1.
incCounter :: MonadMonitor m => Metric Counter -> m ()
incCounter (Metric {handle = MkCounter c}) =
    doIO $ AtomicCounter.incrCounter_ 1 c

-- | Retrieves the current value of a counter metric.
getCounter :: Metric Counter -> IO Int
getCounter (Metric {handle = MkCounter c}) = AtomicCounter.readCounter c

collectCounter :: Info -> AtomicCounter.AtomicCounter -> IO [SampleGroup]
collectCounter info c = do
    value <- AtomicCounter.readCounter c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info CounterType [sample]]
