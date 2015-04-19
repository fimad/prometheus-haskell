module Prometheus.Metric.SummarySpec (
    spec
) where

import Prometheus

import Control.Applicative ((<$>))
import Control.Monad
import Data.List (sortBy)
import System.Random.Shuffle
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Summary" $ do
    let windowSize = 10000
    it "computes quantiles correctly for [0,10000) in order" $ do
        m <- summary (Info "name" "help") quantiles
        mapM_ (`observe` m) [0..(windowSize - 1)]
        checkQuantiles windowSize =<< getQuantiles quantiles m
    it "computes quantiles correctly for [0,10000) in random" $ do
        m <- summary (Info "name" "help") quantiles
        observations <- shuffleM [0..(windowSize - 1)]
        mapM_ (`observe` m) observations
        checkQuantiles windowSize =<< getQuantiles quantiles m

checkQuantiles :: Double -> [(Double, Double, Double)] -> IO ()
checkQuantiles windowSize values =
    forM_ values $ \(q, e, actual) -> do
        let expected = q * (windowSize - 1)
        let minBound = (q - e) * windowSize
        let maxBound = (q + e) * windowSize
        unless (minBound <= actual && actual <= maxBound) $
            expectationFailure $ concat [
                    "Expected value for quantile ", show q
                ,   " was not within acceptable error range. "
                ,   "Got ", show actual, ", but wanted ", show expected
                ,   " (", show minBound, " <= v <= ", show maxBound, ")."
                ]

quantiles :: [Quantile]
quantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]

getQuantiles :: [Quantile] -> Metric Summary -> IO [(Double, Double, Double)]
getQuantiles quantiles summary = do
    values <- sortQuantiles <$> getSummary summary
    let sortedQuantiles = sortQuantiles quantiles
    return $ zipWith (\(q, e) (_, v) -> (q, e, v)) sortedQuantiles values
    where
        sortQuantiles = sortBy (\(a, _) (b, _) -> compare a b)
