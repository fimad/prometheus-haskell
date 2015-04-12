module Prometheus.Metric.TVar (
    collectTVar
) where

import Prometheus.Info
import Prometheus.Metric

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.UTF8 as BS


collectTVar :: Show s => Info -> Type -> STM.TVar s -> IO [Sample]
collectTVar info ty valueTVar = STM.atomically $ do
    value <- STM.readTVar valueTVar
    return [Sample info ty [([], BS.fromString $ show value)]]
