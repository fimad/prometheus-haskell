{-# language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Prometheus.Metric (
    Metric (..)
,   metricIO
,   Sample (..)
,   SampleGroup (..)
,   SampleType (..)
) where

import Prometheus.Info
import Prometheus.Label

import Control.DeepSeq
import qualified Data.ByteString as BS
import Data.Text (Text)


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
data Sample = Sample Text LabelPairs BS.ByteString
    deriving (Show)

-- | A Sample group is a list of samples that is tagged with meta data
-- including the name, help string, and type of the sample.
data SampleGroup = SampleGroup Info SampleType [Sample]
    deriving (Show)

-- | A metric represents a single value that is being monitored. It is comprised
-- of a handle value and a collect method. The handle value is typically a new
-- type wrapped value that provides access to the internal state of the metric.
-- The collect method samples the current value of the metric.
newtype Metric s =
  Metric
    { -- | 'construct' is an 'IO' action that creates a new instance of a metric.
      -- For example, in a counter, this 'IO' action would create a mutable reference
      -- to maintain the state of the counter.
      --
      -- 'construct' returns two things:
      --
      -- 1. The state of the metric itself, which can be used to modify the
      --    metric. A counter would return state pointing to the mutable
      --    reference.
      -- 2. An 'IO' action that samples the metric and returns 'SampleGroup's.
      --    This is the data that will be stored by Prometheus. 
      construct :: IO (s, IO [SampleGroup])
    }
   deriving (Functor)

-- | Use this Applicative instance to combine metrics, in order to export
-- a group of metric as a single value.
instance Applicative Metric where
  pure x = Metric $ return (x, return [])
  (Metric iof) <*> (Metric iox) = Metric $ do
    (f, fsampling) <- iof
    (x, xsampling) <- iox
    return (f x, fsampling <> xsampling)

instance NFData a => NFData (Metric a) where
  rnf (Metric a) = a `seq` ()

-- | What should be the name of this function?
metricIO :: IO (Metric a) -> Metric a
metricIO iometric = Metric $ do
  Metric create <- iometric
  create
