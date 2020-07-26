{-# language BangPatterns #-}
{-# language OverloadedStrings #-}

module Prometheus.Metric.Histogram (
    Histogram
,   Bucket
,   histogram
,   defaultBuckets
,   exponentialBuckets
,   linearBuckets

-- * Exported for testing
,   BucketCounts(..)
,   insert
,   emptyCounts
,   getHistogram
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.Observer
import Prometheus.MonadMonitor

import Control.Applicative ((<$>))
import qualified Control.Concurrent.STM as STM
import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)

-- | A histogram. Counts the number of observations that fall within the
-- specified buckets.
newtype Histogram = MkHistogram (STM.TVar BucketCounts)

instance NFData Histogram where
  rnf (MkHistogram a) = seq a ()

-- | Create a new 'Histogram' metric with a given name, help string, and
-- list of buckets. Panics if the list of buckets is not strictly increasing.
-- A good default list of buckets is 'defaultBuckets'. You can also create
-- buckets with 'linearBuckets' or 'exponentialBuckets'.
histogram :: Info -> [Bucket] -> Metric Histogram
histogram info buckets = Metric $ do
  countsTVar <- STM.newTVarIO  (emptyCounts buckets)
  return (MkHistogram countsTVar, collectHistogram info countsTVar)

-- | Upper-bound for a histogram bucket.
type Bucket = Double

-- | Current state of a histogram.
data BucketCounts = BucketCounts {
    -- | The sum of all the observations.
    histTotal :: !Double
    -- | The number of observations that have been made.
,   histCount :: !Int
    -- | Counts for each bucket. The key is the upper-bound,
    -- value is the number of observations less-than-or-equal-to
    -- that upper bound, but greater than the next lowest upper bound.
,   histCountsPerBucket :: !(Map.Map Bucket Int)
} deriving (Show, Eq, Ord)

emptyCounts :: [Bucket] -> BucketCounts
emptyCounts buckets
    | isStrictlyIncreasing buckets = BucketCounts 0 0 $ Map.fromList (zip buckets (repeat 0))
    | otherwise = error ("Histogram buckets must be in increasing order, got: " ++ show buckets)
    where
         isStrictlyIncreasing xs = and (zipWith (<) xs (tail xs))

instance Observer Histogram where
    -- | Add a new observation to a histogram metric.
    observe h v = withHistogram h (insert v)

-- | Transform the contents of a histogram.
withHistogram :: MonadMonitor m
              => Histogram -> (BucketCounts -> BucketCounts) -> m ()
withHistogram (MkHistogram !bucketCounts) f =
  doIO $ STM.atomically $ STM.modifyTVar' bucketCounts f

-- | Retries a map of upper bounds to counts of values observed that are
-- less-than-or-equal-to that upper bound, but greater than any other upper
-- bound in the map.
getHistogram :: MonadIO m => Histogram -> m (Map.Map Bucket Int)
getHistogram (MkHistogram bucketsTVar) =
    liftIO $ histCountsPerBucket <$> STM.atomically (STM.readTVar bucketsTVar)

-- | Record an observation.
insert :: Double -> BucketCounts -> BucketCounts
insert value BucketCounts { histTotal = total, histCount = count, histCountsPerBucket = counts } =
    BucketCounts (total + value) (count + 1) incCounts
    where
        incCounts =
            case Map.lookupGE value counts of
                Nothing -> counts
                Just (upperBound, _) -> Map.adjust (+1) upperBound counts

-- | Collect the current state of a histogram.
collectHistogram :: Info -> STM.TVar BucketCounts -> IO [SampleGroup]
collectHistogram info bucketCounts = STM.atomically $ do
    BucketCounts total count counts <- STM.readTVar bucketCounts
    let sumSample = Sample (name <> "_sum") [] (bsShow total)
    let countSample = Sample (name <> "_count") [] (bsShow count)
    let infSample = Sample (name <> "_bucket") [(bucketLabel, "+Inf")] (bsShow count)
    let samples = map toSample (cumulativeSum (Map.toAscList counts))
    return [SampleGroup info HistogramType $ samples ++ [infSample, sumSample, countSample]]
    where
        toSample (upperBound, count') =
            Sample (name <> "_bucket") [(bucketLabel, formatFloat upperBound)] $ bsShow count'
        name = metricName info

        -- We don't particularly want scientific notation, so force regular
        -- numeric representation instead.
        formatFloat x = T.pack (showFFloat Nothing x "")

        cumulativeSum xs = zip (map fst xs) (scanl1 (+) (map snd xs))

        bsShow :: Show s => s -> BS.ByteString
        bsShow = BS.fromString . show

-- | The label that defines the upper bound of a bucket of a histogram. @"le"@
-- is short for "less than or equal to".
bucketLabel :: Text
bucketLabel = "le"

-- | The default Histogram buckets. These are tailored to measure the response
-- time (in seconds) of a network service. You will almost certainly need to
-- customize them for your particular use case.
defaultBuckets :: [Double]
defaultBuckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]

-- | Create @count@ buckets, each @width@ wide, where the lowest bucket has an
-- upper bound of @start@. Use this to create buckets for 'histogram'.
linearBuckets :: Bucket -> Double -> Int -> [Bucket]
linearBuckets start width count
    | count <= 0 = error ("Must provide a positive number of linear buckets, got: " ++ show count)
    | otherwise = take count (iterate (width+) start)

-- | Create @count@ buckets, where the lowest bucket has an upper bound of @start@
-- and each bucket's upper bound is @factor@ times the previous bucket's upper bound.
-- Use this to create buckets for 'histogram'.
exponentialBuckets :: Bucket -> Double -> Int -> [Bucket]
exponentialBuckets start factor count
    | count <= 0 = error ("Must provide a positive number of exponential buckets, got: " ++ show count)
    | factor <= 1 = error ("Exponential buckets must have factor greater than 1 to ensure upper bounds are monotonically increasing, got: " ++ show factor)
    | start <= 0 = error ("Exponential buckets must have positive number for start bucket to ensure upper bounds are monotonically increasing, got: " ++ show start)
    | otherwise = take count (iterate (factor*) start)
