module Prometheus.Registry (
    register
,   unsafeRegister
,   collectMetrics
,   unregisterAll
) where

import Prometheus.Metric

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM


data RegisteredMetric = forall s. MkRegisteredMetric (Metric s)

type Registry = [RegisteredMetric]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

register :: MetricGen s -> IO (Metric s)
register desc = do
    metric <- desc
    let addToRegistry = (MkRegisteredMetric metric :)
    STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return metric

unsafeRegister :: MetricGen s -> Metric s
unsafeRegister = unsafePerformIO . register

unregisterAll :: IO ()
unregisterAll = STM.atomically $ STM.writeTVar globalRegistry []

collectMetrics :: IO [SampleGroup]
collectMetrics = do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    concat <$> mapM collectRegistered registry

collectRegistered :: RegisteredMetric -> IO [SampleGroup]
collectRegistered (MkRegisteredMetric metric) = collect metric
