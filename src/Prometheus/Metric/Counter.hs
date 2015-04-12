module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.TVar
import Prometheus.MonadMetric

import qualified Control.Concurrent.STM as STM


newtype Counter = MkCounter (STM.TVar Integer)

counter :: Info -> MetricDesc Counter
counter info = do
    valueTVar <- STM.newTVarIO 0
    return Metric {
            handle = MkCounter valueTVar
        ,   collect = collectTVar info CounterType valueTVar
        }

withCounter :: MonadMetric m => Metric Counter -> (Integer -> Integer) -> m ()
withCounter (Metric {handle = MkCounter valueTVar}) f =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f

incCounter :: MonadMetric m => Metric Counter -> m ()
incCounter counter = withCounter counter inc
    where inc i = i + 1
