module Prometheus.Metric.Summary (
    Summary
,   Quantile
,   summary
,   defaultQuantiles
,   observe
,   getSummary

,   dumpEstimator

,   Estimator (..)
,   Item (..)
,   insert
,   compress
,   query
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.MonadMonitor

import Data.Int (Int64)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.UTF8 as BS


newtype Summary = MkSummary (STM.TVar Estimator)

-- | Creates a new summary metric with a given name, help string, and a list of
-- quantiles. A reasonable set set of quantiles is provided by
-- 'defaultQuantiles'.
summary :: Info -> [Quantile] -> IO (Metric Summary)
summary info quantiles = do
    valueTVar <- STM.newTVarIO (emptyEstimator quantiles)
    return Metric {
            handle = MkSummary valueTVar
        ,   collect = collectSummary info valueTVar
        }

withSummary :: MonadMonitor m
            => Metric Summary -> (Estimator -> Estimator) -> m ()
withSummary (Metric {handle = MkSummary valueTVar}) f =
    doIO $ STM.atomically $ do
        STM.modifyTVar' valueTVar compress
        STM.modifyTVar' valueTVar f

-- | Adds a new observation to a summary metric.
observe :: MonadMonitor m => Double -> Metric Summary -> m ()
observe v s = withSummary s (insert v)

-- | Retrieves a list of tuples containing a quantile and its associated value.
getSummary :: Metric Summary -> IO [(Double, Double)]
getSummary (Metric {handle = MkSummary valueTVar}) = do
    estimator <- STM.atomically $ do
        STM.modifyTVar' valueTVar compress
        STM.readTVar valueTVar
    let quantiles = map fst $ estQuantiles estimator
    let values = map (query estimator) quantiles
    return $ zip quantiles values

collectSummary :: Info -> STM.TVar Estimator -> IO [SampleGroup]
collectSummary info valueTVar = STM.atomically $ do
    STM.modifyTVar' valueTVar compress
    estimator@(Estimator count itemSum _ _) <- STM.readTVar valueTVar
    let quantiles = map fst $ estQuantiles estimator
    let samples =  map (toSample estimator) quantiles
    let sumSample = Sample (metricName info ++ "_sum") [] (bsShow itemSum)
    let countSample = Sample (metricName info ++ "_count") [] (bsShow count)
    return [SampleGroup info SummaryType $ samples ++ [sumSample, countSample]]
    where
        bsShow :: Show s => s -> BS.ByteString
        bsShow = BS.fromString . show

        toSample estimator q = Sample (metricName info) [("quantile", show q)]
                             $ bsShow $ query estimator q

dumpEstimator :: Metric Summary -> IO Estimator
dumpEstimator (Metric {handle = MkSummary valueTVar}) =
    STM.atomically $ STM.readTVar valueTVar

-- | A quantile is a pair of a quantile value and an associated acceptable error
-- value.
type Quantile = (Double, Double)

data Item = Item {
    itemValue :: Double
,   itemG     :: Double
,   itemD     :: Double
} deriving (Eq, Show)

instance Ord Item where
    compare a b = itemValue a `compare` itemValue b

data Estimator = Estimator {
    estCount      :: !Int64
,   estSum        :: !Double
,   estQuantiles  :: [Quantile]
,   estItems      :: [Item]
} deriving (Show)

defaultQuantiles :: [Quantile]
defaultQuantiles = [(0.5, 0.05), (0.9, 0.01), (0.99, 0.001)]

emptyEstimator :: [Quantile] -> Estimator
emptyEstimator quantiles = Estimator 0 0 quantiles []

insert :: Double -> Estimator -> Estimator
insert value estimator@(Estimator oldCount oldSum quantiles items) =
        newEstimator $ insertItem 0 items
    where
        newEstimator = Estimator (oldCount + 1) (oldSum + value) quantiles

        insertItem _ []            = [Item value 1 0]
        insertItem r [x]
            | r == 0               = Item value 1 0 : [x]
            | otherwise            = x : [Item value 1 0]
        insertItem r (x:y:xs)
            | value <= itemValue x = Item value 1 0 : x : y : xs
            | value <= itemValue y = x : Item value 1 (calcD r) : y : xs
            | otherwise            = x : insertItem (itemG x + r) (y : xs)

        calcD r = fromIntegral
                $ floor (invariant estimator {
                    estCount = 1 + estCount estimator
                } r) - (1 :: Int64)


compress :: Estimator -> Estimator
compress est@(Estimator _ _ _ items) = est {
        estItems = compressItems [] items
    }
    where
        compressItems prev []  = reverse prev
        compressItems prev [a] = reverse $ a : prev
        compressItems prev (i1@(Item _ g1 _) : i2@(Item v2 g2 d2) : xs)
            | g1 + g2 + d2 < inv = compressItems prev (Item v2 (g1 + g2) d2:xs)
            | otherwise          = compressItems (i1:prev) (i2:xs)
            where
                r1 = sum $ map itemG prev
                inv = invariant est r1

query :: Estimator -> Double -> Double
query est@(Estimator count _ _ items) q = findQuantile allRs items
    where
        allRs = 0 : zipWith (+) allRs (map itemG items)

        n = fromIntegral count
        f = invariant est

        bound = q * n + f (q * n) / 2

        findQuantile _        []  = 0 / 0  -- NaN
        findQuantile _        [a] = itemValue a
        findQuantile (_:r:rs) (a:b@(Item _ g d):xs)
            | r + g + d > bound   = itemValue a
            | otherwise           = findQuantile (r:rs) (b:xs)
        findQuantile _        _   = error "Query impossibility"

invariant :: Estimator -> Double -> Double
invariant (Estimator count _ quantiles _) r = minimum $ map fj quantiles
    where
        n = fromIntegral count
        fj (q, e) | q * n <= r && r <= n = 2 * e * r / q
                  | otherwise            = 2 * e * (n - r) / (1 - q)
