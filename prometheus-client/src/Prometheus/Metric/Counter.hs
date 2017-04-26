module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
,   addCounter
,   unsafeAddCounter
,   addDurationToCounter
,   getCounter
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.MonadMonitor

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Atomics as Atomics
import qualified Data.ByteString.UTF8 as BS
import qualified Data.IORef as IORef


newtype Counter = MkCounter (IORef.IORef Double)

-- | Creates a new counter metric with a given name and help string.
counter :: Info -> IO (Metric Counter)
counter info = do
    ioref <- IORef.newIORef 0
    return Metric {
            handle = MkCounter ioref
        ,   collect = collectCounter info ioref
        }

withCounter :: MonadMonitor m
          => Metric Counter
          -> (Double -> Double)
          -> m ()
withCounter Metric {handle = MkCounter ioref} f =
    doIO $ Atomics.atomicModifyIORefCAS_ ioref f

-- | Increments the value of a counter metric by 1.
incCounter :: MonadMonitor m => Metric Counter -> m ()
incCounter c = withCounter c (+ 1)

-- | Add the given value to the counter, if it is zero or more.
addCounter :: MonadMonitor m => Double -> Metric Counter -> m Bool
addCounter x c
  | x < 0 = pure False
  | otherwise = do
      withCounter c add
      pure True
  where add i = i `seq` x `seq` i + x

-- | Add the given value to the counter. Panic if it is less than zero.
unsafeAddCounter :: MonadMonitor m => Double -> Metric Counter -> m ()
unsafeAddCounter x c = do
  added <- addCounter x c
  if added
    then pure ()
    else error $ "Tried to add negative value to counter: " ++ show x

-- | Add the duration of an IO action (in seconds) to a counter.
addDurationToCounter :: IO a -> Metric Counter -> IO a
addDurationToCounter io metric = do
    start  <- getCurrentTime
    result <- io
    end    <- getCurrentTime
    addCounter (fromRational $ toRational $ end `diffUTCTime` start) metric
    return result

-- | Retrieves the current value of a counter metric.
getCounter :: Metric Counter -> IO Double
getCounter Metric {handle = MkCounter ioref} = IORef.readIORef ioref

collectCounter :: Info -> IORef.IORef Double -> IO [SampleGroup]
collectCounter info c = do
    value <- IORef.readIORef c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info CounterType [sample]]
