module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
,   getCounter
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.TVar
import Prometheus.MonadMonitor

import Data.Int (Int64)
import qualified Control.Concurrent.STM as STM


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

newtype Counter = MkCounter (STM.TVar Int64)

counter :: Info -> IO (Metric Counter)
counter info = do
    valueTVar <- STM.newTVarIO 0
    return Metric {
            handle = MkCounter valueTVar
        ,   collect = collectTVar info CounterType valueTVar
        }

withCounter :: MonadMonitor m => Metric Counter -> (Int64 -> Int64) -> m ()
withCounter (Metric {handle = MkCounter valueTVar}) f =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f

incCounter :: MonadMonitor m => Metric Counter -> m ()
incCounter counter = withCounter counter inc
    where inc i = i + 1

getCounter :: Metric Counter -> IO Int64
getCounter (Metric {handle = MkCounter valueTVar}) =
    STM.atomically $ STM.readTVar valueTVar
