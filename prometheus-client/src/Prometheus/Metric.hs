{-# language GeneralizedNewtypeDeriving #-}

module Prometheus.Metric (
    Metric (..)
,   Sample (..)
,   SampleGroup (..)
,   SampleType (..)
) where

import Prometheus.Info
import Prometheus.Label

import Control.DeepSeq
import qualified Data.ByteString as BS


-- | The type of a sample. This corresponds to the 5 types of metrics supported
-- by Prometheus.
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

-- | A single value recorded at a moment in time. The sample type contains the
-- name of the sample, a list of labels and their values, and the value encoded
-- as a ByteString.
data Sample = Sample String LabelPairs BS.ByteString
    deriving (Show)

-- | A Sample group is a list of samples that is tagged with meta data
-- including the name, help string, and type of the sample.
data SampleGroup = SampleGroup Info SampleType [Sample]
    deriving (Show)

-- | A metric represents a single value that is being monitored. It is comprised
-- of a handle value and a collect method. The handle value is typically a new
-- type wrapped value that provides access to the internal state of the metric.
-- The collect method samples the current value of the metric.
data Metric s = Metric {
        handle  :: s
    ,   collect :: IO [SampleGroup]
    }

instance NFData a => NFData (Metric a) where
  rnf (Metric a b) = rnf a `seq` b `seq` ()
