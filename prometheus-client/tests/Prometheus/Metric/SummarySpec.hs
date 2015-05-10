module Prometheus.Metric.SummarySpec (
    spec

,   prop_boundedRank
,   prop_invariant
,   rankOf
) where

import Prometheus hiding (collect)
import Prometheus.Metric.Summary

import Control.Applicative ((<$>))
import Control.Monad
import Data.Int (Int64)
import Data.List (sort, sortBy)
import System.Random.Shuffle
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Prometheus.Metric.Summary" $ do
    let windowSize = 10000
    it "computes quantiles correctly for [0,10000) in order" $ do
        m <- summary (Info "name" "help") quantiles
        mapM_ (`observe` m) [0..(windowSize - 1)]
        checkQuantiles windowSize =<< getQuantiles quantiles m
    it "computes quantiles correctly for [0,10000) in random order" $
        replicateM_ 100 $ do
            m <- summary (Info "name" "help") quantiles
            observations <- shuffleM [0..(windowSize - 1)]
            mapM_ (`observe` m) observations
            checkQuantiles windowSize =<< getQuantiles quantiles m
    context "Maintains invariants" invariantTests

checkQuantiles :: Double -> [(Double, Double, Double)] -> IO ()
checkQuantiles windowSize values =
    forM_ values $ \(q, e, actual) -> do
        let expected = fromIntegral $ (floor $ q * windowSize :: Int)
        let minValue = expected - (e * windowSize)
        let maxValue = expected + (e * windowSize)
        unless (minValue <= actual && actual <= maxValue) $
            expectationFailure $ concat [
                    "Expected value for quantile ", show q
                ,   " was not within acceptable error range (", show e , "). "
                ,   "Got ", show actual, ", but wanted ", show expected
                ,   " (", show minValue, " <= v <= ", show maxValue, ")."
                ]

quantiles :: [Quantile]
quantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]

getQuantiles :: [Quantile] -> Metric Summary -> IO [(Double, Double, Double)]
getQuantiles qs s = do
    values <- sortQuantiles <$> getSummary s
    let sortedQuantiles = sortQuantiles qs
    return $ zipWith (\(q, e) (_, v) -> (q, e, v)) sortedQuantiles values
    where
        sortQuantiles = sortBy (\(a, _) (b, _) -> compare a b)

--------------------------------------------------------------------------------
-- QuickCheck tests

invariantTests :: Spec
invariantTests = do
    it "Maintains g + d is bounded above by the invariant f" $
        property prop_invariant
    it "Compression maintains g + d is bounded above by the invariant f" $
        property prop_invariantAfterCompress
    it "Maintains that rank is bounded by r + g and r + g + d" $
        property prop_boundedRank
    it "Compression maintains that rank is bounded by r + g and r + g + d" $
        property prop_boundedRankAfterCompress

prop_invariant :: NonEmptyList Double -> Property
prop_invariant (NonEmpty events) =
    let estimator = estimatorAfterObserving events
        rvgds  = rvgdsFromEstimator estimator
    in  whenFail (putStrLn $ "[(R, V, G, D)] -> " ++ show rvgds) $
        flip all rvgds $ \(r, _, g, d) ->
        let f = invariant estimator (fromIntegral r)
        in  fromIntegral (g + d) <= f

prop_invariantAfterCompress :: NonEmptyList Double -> Property
prop_invariantAfterCompress (NonEmpty events) =
    let estimator = estimatorAfterObserving events
        rvgds  = rvgdsFromEstimator $ compress estimator
    in  whenFail (putStrLn $ "[(R, V, G, D)] -> " ++ show rvgds) $
        flip all rvgds $ \(r, _, g, d) ->
        let f = invariant estimator (fromIntegral r)
        in  fromIntegral (g + d) <= f

prop_boundedRank :: NonEmptyList Double -> Property
prop_boundedRank (NonEmpty events) =
    let rvgds = rvgdsFromEstimator $ estimatorAfterObserving events
        vs    = map (\(_, v, _, _) -> v) rvgds
    in  whenFail (putStrLn $ "[(R, V, G, D)] -> " ++ show rvgds) $
        flip all rvgds $ \(r, v, g, d) ->
        let (minRankInt, maxRankInt) = rankOf v vs
            minRank = fromIntegral minRankInt
            maxRank = fromIntegral maxRankInt
        in  r + g <= maxRank && minRank <= r + g + d

prop_boundedRankAfterCompress :: NonEmptyList Double -> Property
prop_boundedRankAfterCompress (NonEmpty events) =
    let rvgds = rvgdsFromEstimator $ estimatorAfterObserving events
        vs    = map (\(_, v, _, _) -> v) rvgds
    in  whenFail (putStrLn $ "[(R, V, G, D)] -> " ++ show rvgds) $
        flip all rvgds $ \(r, v, g, d) ->
        let (minRankInt, maxRankInt) = rankOf v vs
            minRank = fromIntegral minRankInt
            maxRank = fromIntegral maxRankInt
        in  r + g <= maxRank && minRank <= r + g + d

rvgdsFromEstimator :: Estimator -> [(Int64, Double, Int64, Int64)]
rvgdsFromEstimator estimator = rvgds
    where
        items     = estItems estimator
        rs        = scanl (+) 0 $ map itemG items
        rvgds     = zipWith (\r (Item v g d) -> (r, v, g, d)) rs items

estimatorAfterObserving :: [Double] -> Estimator
estimatorAfterObserving = foldr insert (emptyEstimator [(0.5, 0)])

-- | Return a tuple that describes the range of that an element's true rank can
-- be in. For example, in the list [0, 0, 0, 1] the result for querying 0 will
-- be (0, 2) and the result for querying 1 will be (3, 3).
rankOf :: (Eq a, Ord a) => a -> [a] -> (Int, Int)
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
