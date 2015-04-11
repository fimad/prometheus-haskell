module Prometheus.Registry (
    register
,   dumpMetrics
) where

import Prometheus.Info
import Prometheus.Metric

import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS


data RegisteredMetric = forall s. MkRegisteredMetric (Metric s)

type Registry = [RegisteredMetric]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

register :: MetricDesc s -> IO (Metric s)
register desc = do
    -- Force the metric descriptor to be evaluated. This allows errors returned
    -- by descriptor creating functions to be thrown on registration instead of
    -- at the first use of the metric.
    metric <- desc `seq` makeMetric desc
    let addToRegistry = (MkRegisteredMetric metric :)
    STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return metric

dumpMetrics :: IO LBS.ByteString
dumpMetrics = do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    dumps <- mapM dumpMetric registry
    return $ LBS.concat $ intersperse (LBS.fromString "\n") dumps

dumpMetric :: RegisteredMetric -> IO LBS.ByteString
dumpMetric (MkRegisteredMetric metric) = do
    state <- STM.atomically $ STM.readTVar (metricState metric)
    return $ prefix `LBS.append` metricDump metric [] info state
    where
        ty = metricType metric
        info = metricInfo metric
        name = metricName info
        help = metricHelp info
        prefix =  LBS.fromString $ unlines [
                "# HELP " ++ name ++ " " ++ help
            ,   "# TYPE " ++ name ++ " " ++ ty
            ]
