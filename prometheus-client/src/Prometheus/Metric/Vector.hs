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
import Prometheus.MonadMonitor

import Control.Applicative ((<$>))
import Data.Traversable (forM)
import qualified Data.Atomics as Atomics
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map


type VectorState l m = (Metric m, Map.Map l (m, IO [SampleGroup]))

data Vector l m = MkVector (IORef.IORef (VectorState l m))

-- | Creates a new vector of metrics given a label.
vector :: Label l => l -> Metric m -> Metric (Vector l m)
vector labels gen = Metric $ do
    ioref <- checkLabelKeys labels $ IORef.newIORef (gen, Map.empty)
    return (MkVector ioref, collectVector labels ioref)

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
collectVector :: Label l => l -> IORef.IORef (VectorState l m) -> IO [SampleGroup]
collectVector keys ioref = do
    (_, metricMap) <- IORef.readIORef ioref
    joinSamples <$> concat <$> mapM collectInner (Map.assocs metricMap)
    where
        collectInner (labels, (_metric, sampleGroups)) =
            map (adjustSamples labels) <$> sampleGroups

        adjustSamples labels (SampleGroup info ty samples) =
            SampleGroup info ty (map (prependLabels labels) samples)

        prependLabels l (Sample name labels value) =
            Sample name (labelPairs keys l ++ labels) value

        joinSamples []                      = []
        joinSamples s@(SampleGroup i t _:_) = [SampleGroup i t (extract s)]

        extract [] = []
        extract (SampleGroup _ _ s:xs) = s ++ extract xs

getVectorWith :: (metric -> IO a)
              -> (Vector label metric)
              -> IO [(label, a)]
getVectorWith f (MkVector valueTVar) = do
    (_, metricMap) <- IORef.readIORef valueTVar
    Map.assocs <$> forM metricMap (f . fst)

-- | Given a label, applies an operation to the corresponding metric in the
-- vector.
withLabel :: (Label label, MonadMonitor m)
          => label
          -> (metric -> IO ())
          -> Vector label metric
          -> m ()
withLabel label f (MkVector ioref) = doIO $ do
    (Metric gen, _) <- IORef.readIORef ioref
    newMetric <- gen
    metric <- Atomics.atomicModifyIORefCAS ioref $ \(_, metricMap) ->
        let maybeMetric = Map.lookup label metricMap
            updatedMap  = Map.insert label newMetric metricMap
        in  case maybeMetric of
                Nothing     -> ((Metric gen, updatedMap), newMetric)
                Just metric -> ((Metric gen, metricMap), metric)
    f (fst metric)

-- | Removes a label from a vector.
removeLabel :: (Label label, MonadMonitor m)
            => Vector label metric -> label -> m ()
removeLabel (MkVector valueTVar) label =
    doIO $ Atomics.atomicModifyIORefCAS_ valueTVar f
    where f (desc, metricMap) = (desc, Map.delete label metricMap)

-- | Removes all labels from a vector.
clearLabels :: (Label label, MonadMonitor m)
            => Vector label metric -> m ()
clearLabels (MkVector valueTVar) =
    doIO $ Atomics.atomicModifyIORefCAS_ valueTVar f
    where f (desc, _) = (desc, Map.empty)
