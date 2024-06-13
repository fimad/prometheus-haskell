{-# LANGUAGE ExistentialQuantification #-}
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
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Data.Map.Internal as Map
import Prometheus.Metric.Vector
import Prometheus.Label


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

-- | A 'Registry' is a list of all registered metrics, currently represented by
-- their sampling functions.
type Registry = [IO [SampleGroup]]
type VectorRegistry l m = [IO [(l,(Vector l m))]]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

vectorRegistry :: STM.TVar (VectorRegistry l m)
vectorRegistry = unsafePerformIO $ STM.newTVarIO []

-- | Registers a metric with the global metric registry.
register :: MonadIO m => Metric s -> m s
register (Metric mk) = liftIO $ do
    (metric, sampleGroups) <- mk
    let addToRegistry = (sampleGroups :)
    liftIO $ STM.atomically $ STM.modifyTVar' globalRegistry addToRegistry
    return metric

registerVector :: (MonadIO m,Label l) => l -> Metric mm -> m (Vector l mm)
registerVector l metric = do
    cv <- collectVectors
    let flag = Map.lookup l cv
    case flag of
      Nothing -> do
                    vec <- register $ vector l $ metric
                    let addToVecR = (pure [(l,vec)] :)
                    liftIO $ STM.atomically $ STM.modifyTVar' vectorRegistry addToVecR
                    pure vec
      Just cv -> pure cv

-- | Registers a metric with the global metric registry.
registerIO :: MonadIO m => m (Metric s) -> m s
registerIO metricGen = metricGen >>= register

-- | Registers a metric with the global metric registry.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
unsafeRegister :: Metric s -> s
unsafeRegister = unsafePerformIO . register

-- | Registers a metric with the global metric registry.
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
unsafeRegisterIO :: IO (Metric s) -> s
unsafeRegisterIO = unsafePerformIO . registerIO

-- | Removes all currently registered metrics from the registry.
unregisterAll :: MonadIO m => m ()
unregisterAll = liftIO $ STM.atomically $ STM.writeTVar globalRegistry []

-- | Collect samples from all currently registered metrics. In typical use cases
-- there is no reason to use this function, instead you should use
-- `exportMetricsAsText` or a convenience library.
--
-- This function is likely only of interest if you wish to export metrics in
-- a non-supported format for use with another monitoring service.
collectMetrics :: MonadIO m => m [SampleGroup]
collectMetrics = liftIO $ do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    concat <$> sequence registry

collectVectors :: (MonadIO m, Label l) => m (Map.Map l (Vector l mm))
collectVectors = liftIO $ do
    registry <- STM.atomically $ STM.readTVar vectorRegistry
    (Prelude.foldl (\acc v -> Map.insert (fst v) (snd v) acc) Map.empty) <$> concat <$> sequence registry

