module Prometheus.Metric.Vector (
    Vector
,   vector
,   withLabel
) where

import Prometheus.Info
import Prometheus.Label
import Prometheus.Metric
import Prometheus.MonadMetric

import Data.List (intersperse)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.Map.Strict as Map


data Vector l m = MkVector {
        vectorKeys      :: l
    ,   vectorInnerDesc :: MetricDesc m
    ,   vectorMetrics   :: Map.Map l m
    }

vector :: Label l
       => (Info -> MetricDesc m) -> l -> Info -> MetricDesc (Vector l m)
vector mkMetric labelKeys info = checkLabelKeys labelKeys MetricDesc {
        descDump    = dumpVector
    ,   descInfo    = info
    ,   descInitial = MkVector labelKeys innerMetric Map.empty
    ,   descType    = descType innerMetric
    }
    where innerMetric = mkMetric info

checkLabelKeys :: Label l => l -> a -> a
checkLabelKeys keys r = foldl check r $ map fst $ labelPairs keys keys
    where check _ "instance" = error "The label 'instance' is reserved."
          check _ "job"      = error "The label 'job' is reserved."
          check _ "quantile" = error "The label 'quantile' is reserved."
          check a _          = a

dumpVector :: Label l => LabelPairs -> Info -> Vector l m -> LBS.ByteString
dumpVector preLabels info (MkVector key desc metrics) =
        LBS.concat $ intersperse (LBS.fromString "\n") dumpedInnerMetrics
    where
        labeledMetrics = Map.assocs metrics
        dumpedInnerMetrics = map dumpInnerMetric labeledMetrics
        dumpInnerMetric (label, metric) =
                descDump desc (preLabels ++ labelPairs key label) info metric

withLabel :: (Label label, MonadMetric m)
          => Metric (Vector label metric)
          -> label
          -> (Metric metric -> m ())
          -> m ()
withLabel metric label f = f innerMetric
    where
        -- Modify the inner state of the vector corresponding to the given
        -- label. This will either apply g to the current value in the map, or
        -- apply g to the initial value and insert it into the map.
        modifyInner g = metricModify metric
                        $ \(MkVector keys desc metrics) -> MkVector {
                                vectorKeys      = keys
                            ,   vectorInnerDesc = desc
                            ,   vectorMetrics   = Map.insertWith
                                                    (\_ old -> g old)
                                                    label
                                                    (g (descInitial desc))
                                                    metrics
                            }

        -- Craft a new metric that when modified will update the value in the
        -- vector's map of inner metric states.
        innerMetric = Metric {metricModify = modifyInner}
