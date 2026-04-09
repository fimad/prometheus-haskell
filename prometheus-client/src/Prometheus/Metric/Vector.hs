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
import Control.DeepSeq
import qualified Data.Text as T
import Data.Traversable (forM)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Concurrent.STM (atomically)
import Control.Monad.Trans (lift)
import qualified StmContainers.Map as Map
import qualified Focus
import qualified ListT
import Data.Hashable


type VectorState l m = (Metric m, Map.Map l (m, IO [SampleGroup]))

data Vector l m = MkVector (VectorState l m)

instance NFData (Vector l m) where
  rnf (MkVector (gen, ioref)) = seq ioref `seq` seq gen ()

-- | Creates a new vector of metrics given a label.
vector :: Label l => l -> Metric m -> Metric (Vector l m)
vector labels gen = Metric $ do
    m <- Map.newIO
    vec <- checkLabelKeys labels $ pure $ (gen, m)
    return (MkVector vec, collectVector labels vec)

checkLabelKeys :: Label l => l -> a -> a
checkLabelKeys keys r = foldl check r $ map (T.unpack . fst) $ labelPairs keys keys
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
collectVector :: Label l => l -> VectorState l m -> IO [SampleGroup]
collectVector keys (_, metricMap) = do
    assocs <- ListT.toList $ Map.listTNonAtomic metricMap
    joinSamples <$> concat <$> mapM collectInner assocs
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

getVectorWith :: Vector label metric
              -> (metric -> IO a)
              -> IO [(label, a)]
getVectorWith (MkVector (_, metricMap)) f = do
    ListT.toList $ do
        (l, (m, _collect)) <- Map.listTNonAtomic metricMap
        a <- lift $ f m
        pure (l, a)


-- | Given a label, applies an operation to the corresponding metric in the
-- vector.
withLabel :: (Hashable label, Label label, MonadMonitor m)
          => Vector label metric
          -> label
          -> (metric -> IO ())
          -> m ()
withLabel (MkVector (gen, metricMap)) label f = doIO $ do
    newMetric <- unsafeInterleaveIO $ construct gen
    metric <-
        atomically $
            Map.focus
                (Focus.alter (\mmetric ->
                    case mmetric of
                        Nothing ->
                            Just newMetric
                        Just metric ->
                            Just metric)
                *> Focus.lookupWithDefault newMetric)
                label
                metricMap

    f (fst metric)

-- | Removes a label from a vector.
removeLabel :: (Hashable label, Label label, MonadMonitor m)
            => Vector label metric -> label -> m ()
removeLabel (MkVector (_, metricMap)) label =
    doIO $ atomically $ Map.delete label metricMap

-- | Removes all labels from a vector.
clearLabels :: (Label label, MonadMonitor m)
            => Vector label metric -> m ()
clearLabels (MkVector (_, metricMap)) =
    doIO $ atomically $ Map.reset metricMap
