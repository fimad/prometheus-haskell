module Prometheus.Metric.Vector (
    Vector (..)
,   vector
,   withLabel
,   removeLabel
,   clearLabels
,   getVectorWith
) where

import Prometheus.Label
import Prometheus.Metric
import Prometheus.MonadMetric

import Data.Traversable (forM)
import Control.Applicative ((<$>))
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map


type VectorState l m = (IO (Metric m), Map.Map l (Metric m))

data Vector l m = MkVector (STM.TVar (VectorState l m))

vector :: Label l => l -> IO (Metric m) -> IO (Metric (Vector l m))
vector labels desc = do
    valueTVar <- checkLabelKeys labels $ STM.newTVarIO (desc, Map.empty)
    return Metric {
            handle  = MkVector valueTVar
        ,   collect = collectVector labels valueTVar
        }

checkLabelKeys :: Label l => l -> a -> a
checkLabelKeys keys r = foldl check r $ map fst $ labelPairs keys keys
    where
        check _ "instance" = error "The label 'instance' is reserved."
        check _ "job"      = error "The label 'job' is reserved."
        check _ "quantile" = error "The label 'quantile' is reserved."
        check a (k:ey)
            | validStart k && all validRest ey = a
            | otherwise = error $ "The label '" ++ (k:ey) ++ "' is not valid."
        check _ []         = error "Empty labels are not allowed."

        validStart c =  ('a' <= c && c <= 'z')
                     || ('A' <= c && c <= 'Z')
                     || c == '_'

        validRest c =  ('a' <= c && c <= 'z')
                    || ('A' <= c && c <= 'Z')
                    || ('0' <= c && c <= '9')
                    || c == '_'

-- TODO(will): This currently makes the assumption that all the types and info
-- for all sample groups returned by a metric's collect method will be the same.
-- It is not clear that this will always be a valid assumption.
collectVector :: Label l => l -> STM.TVar (VectorState l m) -> IO [SampleGroup]
collectVector keys valueTVar = do
    (_, metricMap) <- STM.atomically $ STM.readTVar valueTVar
    joinSamples <$> concat <$> mapM collectInner (Map.assocs metricMap)
    where
        collectInner (labels, metric) =
            map (adjustSamples labels) <$> collect metric

        adjustSamples labels (SampleGroup info ty samples) =
            SampleGroup info ty (map (prependLabels labels) samples)

        prependLabels l (Sample name labels value) =
            Sample name (labelPairs keys l ++ labels) value

        joinSamples []                      = []
        joinSamples s@(SampleGroup i t _:_) = [SampleGroup i t (extract s)]

        extract [] = []
        extract (SampleGroup _ _ s:xs) = s ++ extract xs

getVectorWith :: (Metric metric -> IO a)
              -> Metric (Vector label metric)
              -> IO [(label, a)]
getVectorWith f (Metric {handle = MkVector valueTVar}) = do
    (_, metricMap) <- STM.atomically $ STM.readTVar valueTVar
    Map.assocs <$> forM metricMap f

withLabel :: (Label label, MonadMetric m)
          => label
          -> (Metric metric -> IO ())
          -> Metric (Vector label metric)
          -> m ()
withLabel label f (Metric {handle = MkVector valueTVar}) = doIO $ do
    (desc, _) <- STM.atomically $ STM.readTVar valueTVar
    newMetric <- desc
    metric <- STM.atomically $ do
        (_, metricMap) <- STM.readTVar valueTVar
        let maybeMetric = Map.lookup label metricMap
        case maybeMetric of
            Just metric -> return metric
            Nothing     -> do
                let newValue = (desc, Map.insert label newMetric metricMap)
                STM.writeTVar valueTVar newValue
                return newMetric
    f metric

removeLabel :: (Label label, MonadMetric m)
            => Metric (Vector label metric) -> label -> m ()
removeLabel (Metric {handle = MkVector valueTVar}) label =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f
    where f (desc, metricMap) = (desc, Map.delete label metricMap)

clearLabels :: (Label label, MonadMetric m)
            => Metric (Vector label metric) -> m ()
clearLabels (Metric {handle = MkVector valueTVar}) =
    doIO $ STM.atomically $ STM.modifyTVar' valueTVar f
    where f (desc, _) = (desc, Map.empty)
