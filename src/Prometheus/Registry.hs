module Prometheus.Registry (
    register
,   unsafeRegister
,   dumpMetrics
) where

import Prometheus.Info
import Prometheus.Metric

import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS


data RegisteredMetric = forall s. MkRegisteredMetric (MetricDesc s, STM.TVar s)

type Registry = [RegisteredMetric]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

register :: MetricDesc s -> IO (Metric s)
register desc = do
    -- Force the metric descriptor to be evaluated. This allows errors returned
    -- by descriptor creating functions to be thrown on registration instead of
    -- at the first use of the metric.
    stateTVar <- desc `seq` STM.newTVarIO (descInitial desc)
    let addToRegistry = (MkRegisteredMetric (desc, stateTVar) :)
    STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return $ Metric $ STM.atomically . STM.modifyTVar' stateTVar

unsafeRegister :: MetricDesc s -> Metric s
unsafeRegister = unsafePerformIO . register

dumpMetrics :: IO LBS.ByteString
dumpMetrics = do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    dumps <- mapM dumpMetric registry
    return $ LBS.concat $ intersperse (LBS.fromString "\n") dumps

dumpMetric :: RegisteredMetric -> IO LBS.ByteString
dumpMetric (MkRegisteredMetric (desc, stateTVar)) = do
    state <- STM.atomically $ STM.readTVar stateTVar
    return $ prefix `LBS.append` descDump desc [] info state
    where
        ty = descType desc
        info = descInfo desc
        name = metricName info
        help = metricHelp info
        prefix =  LBS.fromString $ unlines [
                "# HELP " ++ name ++ " " ++ help
            ,   "# TYPE " ++ name ++ " " ++ ty
            ]
