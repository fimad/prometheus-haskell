{-# language OverloadedStrings #-}

module Prometheus.Metric.SummarySpec (
    spec
,   rankOf
) where

import Prometheus
import Prometheus.Metric.Summary

import Control.Applicative ((<$>))
import Control.Monad
import Data.Int (Int64)
import Data.List (sort, sortBy)
import Numeric (fromRat)
import System.Random.Shuffle
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Prometheus.Metric.Summary" $ do
    observeToUnsafe 
    let windowSize = 10000
    it "computes quantiles correctly for [0,10000) in order" $ do
        m <- register $ summary (Info "name" "help") quantiles
        mapM_ (m `observe`) [0..(windowSize - 1)]
        checkQuantiles m windowSize =<< getQuantiles quantiles m
    it "computes quantiles correctly for [0,10000) in random order" $ do
        m <- register $ summary (Info "name" "help") quantiles
        observations <- shuffleM [0..(windowSize - 1)]
        mapM_ (m `observe`) observations
        checkQuantiles m windowSize =<< getQuantiles quantiles m
    checkBadObservations badObservations1
    checkBadObservations badObservations2
    checkBadObservations badObservations3
    checkBadObservations badObservations4
    let smallWindowSize = 100
    replicateM_ 50 $ do
        observations <- runIO $ shuffleM [0..(smallWindowSize - 1)]
        it ("computes quantiles correctly for " ++ show observations) $ do
            m <- register $ summary (Info "name" "help") quantiles
            mapM_ (m `observe`) observations
            checkQuantiles m smallWindowSize =<< getQuantiles quantiles m
    where
      checkBadObservations observations =
        it ("computes quantiles correctly for " ++ show observations) $ do
            let windowSize = fromIntegral $ length observations
            m <- register $ summary (Info "name" "help") quantiles
            mapM_ (m `observe`) observations
            checkQuantiles m windowSize =<< getQuantiles quantiles m

{-# NOINLINE testMetric #-}
testMetric :: Summary
testMetric = do
  unsafeRegister $ summary (Info "test_histogram" "") quantiles
  
observeToUnsafe :: Spec
observeToUnsafe =
  it "Is able to observe to a top-level 'unsafeRegister' metric" $ do
    observe testMetric 1 `shouldReturn` ()


badObservations1 :: [Double]
badObservations1 = [38.0, 17.0, 5.0, 36.0, 85.0, 63.0, 3.0, 24.0, 0.0, 12.0,
                   26.0, 70.0, 50.0, 92.0, 69.0, 20.0, 86.0, 14.0, 88.0, 22.0,
                   75.0, 33.0, 43.0, 6.0, 29.0, 94.0, 83.0, 7.0, 18.0, 74.0,
                   53.0, 31.0, 82.0, 66.0, 84.0, 76.0, 58.0, 10.0, 57.0, 21.0,
                   78.0, 59.0, 55.0, 8.0, 27.0, 16.0, 80.0, 32.0, 52.0, 68.0,
                   54.0, 23.0, 49.0, 95.0, 65.0, 56.0, 67.0, 81.0, 46.0, 9.0,
                   28.0, 71.0, 61.0, 47.0, 19.0, 40.0, 87.0, 1.0, 4.0, 35.0,
                   62.0, 30.0, 99.0, 41.0, 34.0, 72.0, 64.0, 79.0, 2.0, 98.0,
                   45.0, 11.0, 89.0, 90.0, 37.0, 97.0, 13.0, 44.0, 91.0, 93.0,
                   25.0, 42.0, 60.0, 15.0, 51.0, 48.0, 73.0, 77.0, 39.0, 96.0]

badObservations2 :: [Double]
badObservations2 = [13.0, 66.0, 54.0, 42.0, 20.0, 35.0, 70.0, 61.0, 96.0, 82.0,
                   30.0, 2.0, 9.0, 36.0, 26.0, 3.0, 67.0, 8.0, 22.0, 95.0, 72.0,
                   1.0, 84.0, 76.0, 17.0, 7.0, 47.0, 14.0, 48.0, 25.0, 73.0,
                   93.0, 89.0, 23.0, 56.0, 33.0, 87.0, 6.0, 31.0, 19.0, 71.0,
                   46.0, 12.0, 63.0, 18.0, 37.0, 10.0, 99.0, 75.0, 62.0, 4.0,
                   57.0, 59.0, 97.0, 11.0, 80.0, 60.0, 53.0, 83.0, 85.0, 38.0,
                   78.0, 69.0, 16.0, 5.0, 64.0, 68.0, 15.0, 21.0, 94.0, 24.0,
                   28.0, 40.0, 58.0, 88.0, 77.0, 55.0, 43.0, 45.0, 51.0, 98.0,
                   39.0, 32.0, 79.0, 81.0, 65.0, 0.0, 41.0, 29.0, 92.0, 50.0,
                   52.0, 34.0, 74.0, 27.0, 90.0, 86.0, 44.0, 49.0, 91.0]

badObservations3 :: [Double]
badObservations3 = [21.0, 7.0, 65.0, 42.0, 56.0, 60.0, 12.0, 31.0, 63.0, 35.0,
                   74.0, 58.0, 27.0, 23.0, 1.0, 39.0, 88.0, 6.0, 22.0, 43.0,
                   54.0, 71.0, 61.0, 51.0, 14.0, 99.0, 76.0, 55.0, 3.0, 53.0,
                   83.0, 59.0, 90.0, 82.0, 37.0, 95.0, 40.0, 94.0, 89.0, 62.0,
                   17.0, 86.0, 46.0, 69.0, 41.0, 10.0, 79.0, 8.0, 77.0, 78.0,
                   34.0, 80.0, 85.0, 48.0, 29.0, 38.0, 5.0, 50.0, 36.0, 15.0,
                   4.0, 64.0, 16.0, 9.0, 96.0, 97.0, 24.0, 72.0, 91.0, 68.0,
                   70.0, 93.0, 13.0, 75.0, 25.0, 26.0, 2.0, 98.0, 81.0, 28.0,
                   73.0, 45.0, 92.0, 32.0, 87.0, 19.0, 20.0, 84.0, 33.0, 67.0,
                   49.0, 44.0, 11.0, 18.0, 57.0, 52.0, 66.0, 47.0, 0.0, 30.0]

badObservations4 :: [Double]
badObservations4 = [48.0, 64.0, 66.0, 53.0, 82.0, 76.0, 62.0, 10.0, 86.0, 8.0,
                   32.0, 50.0, 91.0, 56.0, 59.0, 93.0, 6.0, 22.0, 98.0, 94.0,
                   89.0, 75.0, 42.0, 39.0, 3.0, 52.0, 73.0, 38.0, 58.0, 81.0,
                   23.0, 69.0, 17.0, 71.0, 33.0, 63.0, 65.0, 25.0, 1.0, 49.0,
                   2.0, 77.0, 16.0, 88.0, 9.0, 96.0, 13.0, 83.0, 36.0, 57.0,
                   80.0, 7.0, 0.0, 68.0, 5.0, 85.0, 60.0, 37.0, 44.0, 67.0,
                   21.0, 90.0, 72.0, 84.0, 11.0, 28.0, 20.0, 79.0, 27.0, 61.0,
                   4.0, 34.0, 24.0, 97.0, 26.0, 54.0, 55.0, 45.0, 51.0, 19.0,
                   87.0, 78.0, 30.0, 40.0, 15.0, 18.0, 43.0, 29.0, 74.0, 14.0,
                   95.0, 46.0, 35.0, 12.0, 31.0, 99.0, 70.0, 41.0, 47.0, 92.0]

checkQuantiles :: Summary
               -> Double
               -> [(Rational, Rational, Double)] -> IO ()
checkQuantiles m windowSize values = do
    forM_ values $ \(q, e, actual) -> do
        let expected = fromIntegral $ (ceiling $ fromRat q * windowSize :: Int) - 1
        let minValue = expected - (fromRat e * windowSize)
        let maxValue = expected + (fromRat e * windowSize)
        unless (minValue <= actual && actual <= maxValue) $
            expectationFailure $ concat [
                    "Expected value for quantile ", show q
                ,   " was not within acceptable error range (", show e , "). "
                ,   "Got ", show actual, ", but wanted ", show expected
                ,   " (", show minValue, " <= v <= ", show maxValue, ")."
                ]

quantiles :: [Quantile]
quantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]

getQuantiles :: [Quantile] -> Summary -> IO [(Rational, Rational, Double)]
getQuantiles qs s = do
    values <- sortQuantiles <$> getSummary s
    let sortedQuantiles = sortQuantiles qs
    return $ zipWith (\(q, e) (_, v) -> (q, e, v)) sortedQuantiles values
    where
        sortQuantiles = sortBy (\(a, _) (b, _) -> compare a b)

-- | Return a tuple that describes the range of that an element's true rank can
-- be in. For example, in the list [0, 0, 0, 1] the result for querying 0 will
-- be (0, 2) and the result for querying 1 will be (3, 3).
rankOf :: (Eq a, Ord a) => a -> [a] -> (Int64, Int64)
rankOf b = rankOfSorted Nothing 1 b . sort
    where
        rankOfSorted Nothing _ _ [] = error "Element not in list"
        rankOfSorted (Just minIndex) index _ [] = (minIndex, index - 1)
        rankOfSorted Nothing index a (x:xs)
            | a == x    = rankOfSorted (Just index) (index + 1) a xs
            | otherwise = rankOfSorted Nothing (index + 1) a xs
        rankOfSorted (Just minIndex) index a (x:xs)
            | a == x    = rankOfSorted (Just minIndex) (index + 1) a xs
            | otherwise = (minIndex, index - 1)
