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
import Prometheus.Metric.TVar
import Prometheus.MonadMetric

import qualified Control.Concurrent.STM as STM


newtype Gauge = MkGauge (STM.TVar Double)

gauge :: Info -> IO (Metric Gauge)
gauge info = do
    valueTVar <- STM.newTVarIO 0
    return Metric {
            handle = MkGauge valueTVar
        ,   collect = collectTVar info GaugeType valueTVar
        }

withGauge :: MonadMetric m
          => Metric Gauge
          -> (Double -> Double)
          -> m ()
withGauge (Metric {handle = MkGauge valueTVar}) f =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f

addGauge :: MonadMetric m => Double -> Metric Gauge -> m ()
addGauge x gauge = withGauge gauge add
    where add i = i `seq` x `seq` i + x

subGauge :: MonadMetric m => Double -> Metric Gauge -> m ()
subGauge x gauge = withGauge gauge sub
    where sub i = i `seq` x `seq` i - x

incGauge :: MonadMetric m => Metric Gauge -> m ()
incGauge gauge = withGauge gauge (+ 1)

decGauge :: MonadMetric m => Metric Gauge -> m ()
decGauge gauge = withGauge gauge (+ (-1))

setGauge :: MonadMetric m => Double -> Metric Gauge -> m ()
setGauge r gauge = withGauge gauge set
    where set _ = r

getGauge :: Metric Gauge -> IO Double
getGauge (Metric {handle = MkGauge valueTVar}) =
    STM.atomically $ STM.readTVar valueTVar
