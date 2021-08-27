{-# language BangPatterns #-}
{-# language OverloadedStrings #-}
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
import qualified Data.Text as T
import DataSketches.Quantiles.RelativeErrorQuantile
import qualified DataSketches.Quantiles.RelativeErrorQuantile as ReqSketch
import Data.Maybe (mapMaybe)
import Prelude hiding (maximum)
import qualified Prelude
import Data.Word

data Summary = MkSummary
  { reqSketch :: MVar (ReqSketch (PrimState IO))
  , quantiles :: [Quantile]
  }

instance NFData Summary where
  rnf (MkSummary a b) = a `seq` b `deepseq` ()


type Quantile = (Rational, Rational)

-- | K is a parameter divisible by two, in the range 4-1024 used in the RelativeErrorQuantile algorithm to 
-- determine how many items must be retained per compaction section. As the value increases, the accuracy
-- of the sketch increases as well. This function iterates on the k value starting from 6 
-- (conservative on space, but reasonably accurate) until it finds a K value that satisfies the specified 
-- error bounds for the given quantile. Note: this algorithm maintains highest accuracy for the upper tail 
-- of the quantile when passed the 'HighRanksAreAccurate', sampling out more items at lower ranks during 
-- the compaction process. Thus, extremely tight error bounds on low quantile values may cause this 
-- function to return 'Nothing'.
--
-- If another smart constructor was exposed for summary creation, specific k values & LowRanksAreAccurate
-- could be used to refine accuracy settings to bias towards lower quantiles when retaining accurate samples.
determineK :: Quantile -> Maybe Word32
determineK (rank_, acceptableError) = go 6
    where
        go k =
            let rse = relativeStandardError (fromIntegral k) (fromRational rank_) HighRanksAreAccurate 50000
            in if abs (rse - fromRational rank_) <= fromRational acceptableError
                then Just k
                else if k < 1024
                    then go (k + 2)
                    else Nothing


-- | Creates a new summary metric with a given name, help string, and a list of
-- quantiles. A reasonable set set of quantiles is provided by
-- 'defaultQuantiles'.
summary :: Info -> [Quantile] -> Metric Summary
summary info quantiles_ = Metric $ do
    rs <- mkReqSketch kInt HighRanksAreAccurate
    mv <- newMVar $ rs {criterion = (:<=)}
    let summary_ = MkSummary mv quantiles_
    return (summary_, collectSummary info summary_)
    where
        kInt = fromIntegral $ case mapMaybe determineK quantiles_ of
          [] -> error "Unable to create a Summary meeting the provided quantile precision requirements"
          xs -> Prelude.maximum xs

instance Observer Summary where
    -- | Adds a new observation to a summary metric.
    observe s v = doIO $ withMVar (reqSketch s) (`ReqSketch.insert` v)

-- | Retrieves a list of tuples containing a quantile and its associated value.
getSummary :: MonadIO m => Summary -> m [(Rational, Double)]
getSummary (MkSummary sketchVar quantiles_) = liftIO $ withMVar sketchVar $ \sketch -> do
  forM quantiles_ $ \qv ->
    (,) <$> pure (fst qv) <*> ReqSketch.quantile sketch (fromRational $ fst qv)

collectSummary :: Info -> Summary -> IO [SampleGroup]
collectSummary info (MkSummary sketchVar quantiles_) = withMVar sketchVar $ \sketch -> do
    itemSum <- ReqSketch.sum sketch
    count_ <- ReqSketch.count sketch
    estimatedQuantileValues <- forM quantiles_ $ \qv ->
      (,) <$> pure (fst qv) <*> ReqSketch.quantile sketch (toDouble $ fst qv)
    let sumSample = Sample (metricName info <> "_sum") [] (bsShow itemSum)
    let countSample = Sample (metricName info <> "_count") [] (bsShow count_)
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