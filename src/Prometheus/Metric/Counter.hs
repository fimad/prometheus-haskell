module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.MonadMetric


newtype Counter = MkCounter Integer

instance Show Counter where
    show (MkCounter i) = show i

counter :: Info -> MetricDesc Counter
counter info = MetricDesc {
        descDump    = defaultMetricDump
    ,   descInfo    = info
    ,   descInitial = MkCounter 0
    ,   descType    = "counter"
    }

incCounter :: MonadMetric m => Metric Counter -> m ()
incCounter counter = doIO $ metricModify counter inc
    where inc (MkCounter i) = (i + 1) `seq` MkCounter (i + 1)
