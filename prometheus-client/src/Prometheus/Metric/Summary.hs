{-# language OverloadedStrings #-}

module Prometheus.Metric.Summary (
    Summary
,   Quantile
,   summary
,   defaultQuantiles
,   observe
,   observeDuration
,   getSummary

,   dumpEstimator
,   emptyEstimator
,   Estimator (..)
,   Item (..)
,   insert
,   compress
,   query
,   invariant
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.Observer
import Prometheus.MonadMonitor

import qualified Control.Concurrent.STM as STM
import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BS
import Data.Foldable (foldr')
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T


newtype Summary = MkSummary (STM.TVar Estimator)

instance NFData Summary where
  rnf (MkSummary a) = a `seq` ()

-- | Creates a new summary metric with a given name, help string, and a list of
-- quantiles. A reasonable set set of quantiles is provided by
-- 'defaultQuantiles'.
summary :: Info -> [Quantile] -> Metric Summary
summary info quantiles = Metric $ do
    valueTVar <- STM.newTVarIO (emptyEstimator quantiles)
    return (MkSummary valueTVar, collectSummary info valueTVar)

withSummary :: MonadMonitor m
            => Summary -> (Estimator -> Estimator) -> m ()
withSummary (MkSummary valueTVar) f =
    doIO $ STM.atomically $ do
        STM.modifyTVar' valueTVar compress
        STM.modifyTVar' valueTVar f

instance Observer Summary where
    -- | Adds a new observation to a summary metric.
    observe s v = withSummary s (insert v)

-- | Retrieves a list of tuples containing a quantile and its associated value.
getSummary :: MonadIO m => Summary -> m [(Rational, Double)]
getSummary (MkSummary valueTVar) = liftIO $ do
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
    let sumSample = Sample (metricName info <> "_sum") [] (bsShow itemSum)
    let countSample = Sample (metricName info <> "_count") [] (bsShow count)
    return [SampleGroup info SummaryType $ samples ++ [sumSample, countSample]]
    where
        bsShow :: Show s => s -> BS.ByteString
        bsShow = BS.fromString . show

        toSample estimator q =
            Sample (metricName info) [("quantile", T.pack . show $ toDouble q)] $
                bsShow $ query estimator q

        toDouble :: Rational -> Double
        toDouble = fromRational

dumpEstimator :: Summary -> IO Estimator
dumpEstimator (MkSummary valueTVar) =
    STM.atomically $ STM.readTVar valueTVar

-- | A quantile is a pair of a quantile value and an associated acceptable error
-- value.
type Quantile = (Rational, Rational)

data Item = Item {
    itemValue :: Double
,   itemG     :: !Int64
,   itemD     :: !Int64
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

        insertItem _ [] = [Item value 1 0]
        insertItem r [x]
            -- The first two cases cover the scenario where the initial size of
            -- the list is one.
            | r == 0 && value < itemValue x = Item value 1 0 : [x]
            | r == 0                        = x : [Item value 1 0]
            -- The last case covers the scenario where the we have walked off
            -- the end of a list with more than 1 element in the final case of
            -- insertItem in which case we already know that x < value.
            | otherwise                     = x : [Item value 1 0]
        insertItem r (x:y:xs)
            -- This first case only covers the scenario where value is less than
            -- the first item in a multi-item list. For subsequent steps of
            -- a multi valued list, this case cannot happen as it would have
            -- fallen through to the case below in the previous step.
            | value <= itemValue x = Item value 1 0 : x : y : xs
            | value <= itemValue y = x : Item value 1 (calcD $ r + itemG x)
                                       : y : xs
            | otherwise            = x : insertItem (itemG x + r) (y : xs)

        calcD r = max 0
                $ floor (invariant estimator (fromIntegral r)) - 1


compress :: Estimator -> Estimator
compress est@(Estimator _ _ _ [])    = est
compress est@(Estimator _ _ _ items) = est {
        estItems = (minItem :)
                 $ foldr' compressPair []
                 $ drop 1  -- The exact minimum item must be kept exactly.
                 $ zip items
                 $ scanl (+) 0 (map itemG items)
    }
    where
        minItem = head items
        compressPair (a, _) [] = [a]
        compressPair (a@(Item _ aG _), r) (b@(Item bVal bG bD):bs)
            | bD == 0             = a : b : bs
            | aG + bG + bD <= inv = Item bVal (aG + bG) bD : bs
            | otherwise           = a : b : bs
            where
                inv = floor $ invariant est (fromIntegral r)

query :: Estimator -> Rational -> Double
query est@(Estimator count _ _ items) q = findQuantile allRs items
    where
        allRs = scanl (+) 0 $ map itemG items

        n = fromIntegral count
        f = invariant est

        rank  = q * n
        bound = rank + (f rank / 2)

        findQuantile _        []   = 0 / 0  -- NaN
        findQuantile _        [a]  = itemValue a
        findQuantile (_:bR:rs) (a@(Item{}):b@(Item _ bG bD):xs)
            | fromIntegral (bR + bG + bD) > bound = itemValue a
            | otherwise            = findQuantile (bR:rs) (b:xs)
        findQuantile _        _    = error "Query impossibility"

invariant :: Estimator -> Rational -> Rational
invariant (Estimator count _ quantiles _) r = max 1
                                            $ minimum $ map fj quantiles
    where
        n = fromIntegral count
        fj (q, e) | q * n <= r = 2 * e * r / q
                  | otherwise  = 2 * e * (n - r) / (1 - q)
