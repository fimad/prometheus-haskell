module Prometheus.Metric.Gauge (
    Gauge
,   gauge
,   incGauge
,   decGauge
,   addGauge
,   subGauge
,   setGauge
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.TVar
import Prometheus.MonadMetric

import qualified Control.Concurrent.STM as STM
import qualified Data.Scientific as Scientific


newtype Gauge = MkGauge (STM.TVar Scientific.Scientific)

gauge :: Info -> MetricDesc Gauge
gauge info = do
    valueTVar <- STM.newTVarIO 0
    return Metric {
            handle = MkGauge valueTVar
        ,   collect = collectTVar info GaugeType valueTVar
        }

withGauge :: MonadMetric m
          => Metric Gauge
          -> (Scientific.Scientific -> Scientific.Scientific)
          -> m ()
withGauge (Metric {handle = MkGauge valueTVar}) f =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f

addGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
addGauge x gauge = withGauge gauge add
    where add i = r `seq` i + r
          r = Scientific.fromFloatDigits x

subGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
subGauge x gauge = withGauge gauge sub
    where sub i = r `seq` i - r
          r = Scientific.fromFloatDigits x

incGauge :: MonadMetric m => Metric Gauge -> m ()
incGauge gauge = withGauge gauge (+ 1)

decGauge :: MonadMetric m => Metric Gauge -> m ()
decGauge gauge = withGauge gauge (+ (-1))

setGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
setGauge r gauge = withGauge gauge set
    where set _ = real
          real = Scientific.fromFloatDigits r
