{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Prometheus.Metric.Summary (
    Summary
,   Quantile
,   summary
,   defaultQuantiles
,   observe
,   observeDuration
,   getSummary
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.Observer
import Prometheus.MonadMonitor

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import qualified Data.ByteString.UTF8 as BS
import Data.Foldable (foldr')
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import DataSketches.Quantiles.RelativeErrorQuantile
import qualified DataSketches.Quantiles.RelativeErrorQuantile as ReqSketch

data Summary = MkSummary 
  { reqSketch :: MVar (ReqSketch 6 (PrimState IO))
  , quantiles :: [Quantile]
  }

instance NFData Summary where
  rnf (MkSummary a b) = a `seq` b `seq` ()


type Quantile = (Rational, Rational)

-- | Creates a new summary metric with a given name, help string, and a list of
-- quantiles. A reasonable set set of quantiles is provided by
-- 'defaultQuantiles'.
summary :: Info -> [Quantile] -> Metric Summary
summary info quantiles = Metric $ do
    -- valueTVar <- STM.newTVarIO (emptyEstimator quantiles)
    mv <- newMVar =<< mkReqSketch @6 HighRanksAreAccurate
    let summary_ = MkSummary mv quantiles
    return (summary_, collectSummary info summary_)

instance Observer Summary where
    -- | Adds a new observation to a summary metric.
    observe s v = doIO $ withMVar (reqSketch s) (\s -> update s v)

-- | Retrieves a list of tuples containing a quantile and its associated value.
getSummary :: MonadIO m => Summary -> m [(Rational, Double)]
getSummary (MkSummary sketchVar quantiles) = liftIO $ withMVar sketchVar $ \sketch -> do
  forM quantiles $ \qv -> 
    (,) <$> pure (fst qv) <*> ReqSketch.quantile sketch (fromRational $ fst qv)

collectSummary :: Info -> Summary -> IO [SampleGroup]
collectSummary info (MkSummary sketchVar quantiles) = withMVar sketchVar $ \sketch -> do
    itemSum <- getSum sketch
    count <- getN sketch
    estimatedQuantileValues <- forM quantiles $ \qv -> 
      (,) <$> pure (fst qv) <*> ReqSketch.quantile sketch (toDouble $ fst qv)
    let sumSample = Sample (metricName info <> "_sum") [] (bsShow itemSum)
    let countSample = Sample (metricName info <> "_count") [] (bsShow count)
    return [SampleGroup info SummaryType $ map toSample estimatedQuantileValues ++ [sumSample, countSample]]
    where
        bsShow :: Show s => s -> BS.ByteString
        bsShow = BS.fromString . show

        toSample :: (Rational, Double) -> Sample
        toSample (q, estimatedValue) =
            Sample (metricName info) [("quantile", T.pack . show $ toDouble q)] $
                bsShow estimatedValue

        toDouble :: Rational -> Double
        toDouble = fromRational

defaultQuantiles :: [Quantile]
defaultQuantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]