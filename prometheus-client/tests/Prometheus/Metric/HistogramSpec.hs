{-# language OverloadedStrings #-}

module Prometheus.Metric.HistogramSpec (
    spec
) where

import Prometheus.Metric.Histogram
import Prometheus.Metric.Observer
import Prometheus.Registry
import Prometheus.Info

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Prometheus.Metric.Histogram" $ do
    context "Maintains invariants" invariantTests
    context "Laziness tests" observeToUnsafe

{-# NOINLINE testMetric #-}
testMetric :: Histogram
testMetric = do
  unsafeRegister $ histogram (Info "test_histogram" "")  defaultBuckets
  
observeToUnsafe :: Spec
observeToUnsafe =
  it "Is able to observe to a top-level 'unsafeRegister' metric" $ do
    observe testMetric 1 `shouldReturn` ()

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
