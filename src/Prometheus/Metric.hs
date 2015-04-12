module Prometheus.Metric (
    Metric (..)
,   MetricDesc (..)
,   defaultMetricDump
) where

import Prometheus.Info
import Prometheus.Label

import Data.List (intercalate)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS


data MetricDesc s = MetricDesc {
        descDump    :: LabelPairs -> Info -> s -> LBS.ByteString
    ,   descInfo    :: Info
    ,   descInitial :: s
    ,   descType    :: String
    }

data Metric s = Metric {
        metricModify :: (s -> s) -> IO ()
    }

defaultMetricDump :: Show s => LabelPairs -> Info -> s -> LBS.ByteString
defaultMetricDump [] info value = LBS.fromString $ concat [
        metricName info, " ", show value
    ]
defaultMetricDump labels info value = LBS.fromString $ concat [
        metricName info, "{", dumpLabels labels, "} ", show value
    ]

dumpLabels :: LabelPairs -> String
dumpLabels labels = intercalate "," $ map dumpLabel labels

dumpLabel :: (String, String) -> String
dumpLabel (key, value) = key ++ "=" ++ show value
