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
import Prometheus.MonadMetric

import qualified Data.Scientific as Scientific


newtype Gauge = MkGauge (Scientific.Scientific)

instance Show Gauge where
    show (MkGauge s) = show s

gauge :: Info -> MetricDesc Gauge
gauge info = MetricDesc {
        descDump    = defaultMetricDump
    ,   descInfo    = info
    ,   descInitial = MkGauge 0
    ,   descType    = "gauge"
    }

addGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
addGauge x gauge = doIO $ metricModify gauge add
    where add (MkGauge i) = r `seq` (i + r) `seq` MkGauge (i + r)
          r = Scientific.fromFloatDigits x

subGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
subGauge x gauge = doIO $ metricModify gauge sub
    where sub (MkGauge i) = r `seq` (i - r) `seq` MkGauge (i - r)
          r = Scientific.fromFloatDigits x

incGauge :: MonadMetric m => Metric Gauge -> m ()
incGauge gauge = doIO $ metricModify gauge inc
    where inc (MkGauge i) = (i + 1) `seq` MkGauge (i + 1)

decGauge :: MonadMetric m => Metric Gauge -> m ()
decGauge gauge = doIO $ metricModify gauge dec
    where dec (MkGauge i) = (i - 1) `seq` MkGauge (i - 1)

setGauge :: (MonadMetric m, RealFloat r) => r -> Metric Gauge -> m ()
setGauge r gauge = doIO $ metricModify gauge set
    where set _ = real `seq` MkGauge real
          real = Scientific.fromFloatDigits r
