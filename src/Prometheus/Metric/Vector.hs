module Prometheus.Metric.Vector (
    Vector (..)
,   vector
,   withLabel
,   removeLabel
,   clearLabels
) where

import Prometheus.Label
import Prometheus.Metric
import Prometheus.MonadMetric

import Control.Applicative ((<$>))
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map


type VectorState l m = (MetricDesc m, Map.Map l (Metric m))

data Vector l m = MkVector (STM.TVar (VectorState l m))

vector :: Label l => MetricDesc m -> l -> MetricDesc (Vector l m)
vector desc labels = do
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

collectVector :: Label l => l -> STM.TVar (VectorState l m) -> IO [Sample]
collectVector keys valueTVar = do
    (_, metricMap) <- STM.atomically $ STM.readTVar valueTVar
    joinSamples <$> concat <$> mapM collectInner (Map.assocs metricMap)
    where
        collectInner (labels, metric) =   map (adjustSamples labels)
                                      <$> collect metric

        adjustSamples labels (Sample info ty samples) =
            Sample info ty (map (prependLabels labels) samples)

        prependLabels l (labels, value) = (labelPairs keys l ++ labels, value)

        joinSamples []                       = []
        joinSamples s@(Sample info ty _:_) = [Sample info ty (extract s)]

        extract [] = []
        extract (Sample _ _ s:xs) = s ++ extract xs

withLabel :: (Label label, MonadMetric m)
          => Metric (Vector label metric)
          -> label
          -> (Metric metric -> IO ())
          -> m ()
withLabel (Metric {handle = MkVector valueTVar}) label f = doIO $ do
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
