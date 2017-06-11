module Prometheus.Metric.HistogramSpec (
    spec
) where

import Prometheus.Metric.Histogram

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Prometheus.Metric.Histogram" $ do
    context "Maintains invariants" invariantTests

--------------------------------------------------------------------------------
-- QuickCheck tests

invariantTests :: Spec
invariantTests = do
    it "Total count is greater than or equal to sum of counts per bucket." $
      property prop_totalCountVsCountPerBucket

prop_totalCountVsCountPerBucket :: [Double] -> Bool
prop_totalCountVsCountPerBucket observations =
    let bucketCounts = bucketCountsAfterObserving observations
    in  sum (Map.elems (histCountsPerBucket bucketCounts)) <= histCount bucketCounts

bucketCountsAfterObserving :: [Double] -> BucketCounts
bucketCountsAfterObserving = foldr insert (emptyCounts defaultBuckets)
