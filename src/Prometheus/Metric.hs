module Prometheus.Metric (
    Metric (..)
,   MetricDesc
,   Sample (..)
,   Type (..)
) where

import Prometheus.Info
import Prometheus.Label

import qualified Data.ByteString as BS


data Type = CounterType | GaugeType | SummaryType | HistogramType | UntypedType

instance Show Type where
    show CounterType   = "counter"
    show GaugeType     = "gauge"
    show SummaryType   = "summary"
    show HistogramType = "histogram"
    show UntypedType   = "untyped"

data Sample = Sample Info Type [(LabelPairs, BS.ByteString)]
    deriving (Show)

type MetricDesc s = IO (Metric s)

data Metric s = Metric {
        handle  :: s
    ,   collect :: IO [Sample]
    }
