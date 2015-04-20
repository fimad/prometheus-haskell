module Prometheus.Registry (
    register
,   registerIO
,   unsafeRegister
,   unsafeRegisterIO
,   collectMetrics
,   unregisterAll
) where

import Prometheus.Metric

import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

data RegisteredMetric = forall s. MkRegisteredMetric (Metric s)

type Registry = [RegisteredMetric]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

-- | Registers a metric with the global metric registry or retrieves the
-- currently registered metric with the same description.
register :: Metric s -> IO (Metric s)
register metric = do
    let addToRegistry = (MkRegisteredMetric metric :)
    STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return metric

-- | Registers a metric with the global metric registry or retrieves the
-- currently registered metric with the same description.
registerIO :: IO (Metric s) -> IO (Metric s)
registerIO metricGen = metricGen >>= register

-- | Registers a metric with the global metric registry or retrieves the
-- currently registered metric with the same description.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
unsafeRegister :: Metric s -> Metric s
unsafeRegister = unsafePerformIO . register

-- | Registers a metric with the global metric registry or retrieves the
-- currently registered metric with the same description.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
--
-- For example,
--
-- >>> :{
--  {-# NOINLINE c #-}
--  let c = unsafeRegisterIO $ counter (Info "my_counter" "An example metric")
-- :}
-- ...
unsafeRegisterIO :: IO (Metric s) -> Metric s
unsafeRegisterIO = unsafePerformIO . registerIO

-- | Removes all currently registered metrics from the registry.
unregisterAll :: IO ()
unregisterAll = STM.atomically $ STM.writeTVar globalRegistry []

-- | Collect samples from all currently registered metrics. In typical use cases
-- there is no reason to use this function, instead you should use
-- `exportMetricsAsText` or a convenience library.
--
-- This function is likely only of interest if you wish to export metrics in
-- a non-supported format for use with another monitoring service.
collectMetrics :: IO [SampleGroup]
collectMetrics = do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    concat <$> mapM collectRegistered registry

collectRegistered :: RegisteredMetric -> IO [SampleGroup]
collectRegistered (MkRegisteredMetric metric) = collect metric
