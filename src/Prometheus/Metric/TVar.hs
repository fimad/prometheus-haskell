module Prometheus.Metric.TVar (
    collectTVar
) where

import Prometheus.Info
import Prometheus.Metric

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.UTF8 as BS


collectTVar :: Show s => Info -> Type -> STM.TVar s -> IO [SampleGroup]
collectTVar info ty valueTVar = STM.atomically $ do
    value <- STM.readTVar valueTVar
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info ty [sample]]
