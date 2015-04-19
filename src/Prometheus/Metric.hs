module Prometheus.Metric (
    Metric (..)
,   Sample (..)
,   SampleGroup (..)
,   SampleType (..)
) where

import Prometheus.Info
import Prometheus.Label

import qualified Data.ByteString as BS


data SampleType
    = CounterType
    | GaugeType
    | SummaryType
    | HistogramType
    | UntypedType

instance Show SampleType where
    show CounterType   = "counter"
    show GaugeType     = "gauge"
    show SummaryType   = "summary"
    show HistogramType = "histogram"
    show UntypedType   = "untyped"

data Sample = Sample String LabelPairs BS.ByteString
    deriving (Show)

data SampleGroup = SampleGroup Info SampleType [Sample]
    deriving (Show)

data Metric s = Metric {
        handle  :: s
    ,   collect :: IO [SampleGroup]
    }
