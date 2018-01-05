module Prometheus.Metric.Counter (
    Counter
,   counter
,   incCounter
,   addCounter
,   unsafeAddCounter
,   addDurationToCounter
,   getCounter
,   countExceptions
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Metric.Observer (timeAction)
import Prometheus.MonadMonitor

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad (unless)
import qualified Data.Atomics as Atomics
import qualified Data.ByteString.UTF8 as BS
import qualified Data.IORef as IORef


newtype Counter = MkCounter (IORef.IORef Double)

instance NFData Counter where
  rnf (MkCounter ioref) = seq ioref ()

-- | Creates a new counter metric with a given name and help string.
counter :: Info -> Metric Counter
counter info = Metric $ do
    ioref <- IORef.newIORef 0
    return (MkCounter ioref, collectCounter info ioref)

withCounter :: MonadMonitor m
          => Counter
          -> (Double -> Double)
          -> m ()
withCounter (MkCounter ioref) f =
    doIO $ Atomics.atomicModifyIORefCAS_ ioref f

-- | Increments the value of a counter metric by 1.
incCounter :: MonadMonitor m => Counter -> m ()
incCounter c = withCounter c (+ 1)

-- | Add the given value to the counter, if it is zero or more.
addCounter :: MonadMonitor m => Counter -> Double -> m Bool
addCounter c x
  | x < 0 = return False
  | otherwise = do
      withCounter c add
      return True
  where add i = i `seq` x `seq` i + x

-- | Add the given value to the counter. Panic if it is less than zero.
unsafeAddCounter :: MonadMonitor m => Counter -> Double -> m ()
unsafeAddCounter c x = do
  added <- addCounter c x
  unless added $
    error $ "Tried to add negative value to counter: " ++ show x

-- | Add the duration of an IO action (in seconds) to a counter.
--
-- If the IO action throws, no duration is added.
addDurationToCounter :: (MonadIO m, MonadMonitor m) => Counter -> m a -> m a
addDurationToCounter metric io = do
    (result, duration) <- timeAction io
    _ <- addCounter metric duration 
    return result

-- | Retrieves the current value of a counter metric.
getCounter :: MonadIO m => Counter -> m Double
getCounter (MkCounter ioref) = liftIO $ IORef.readIORef ioref

collectCounter :: Info -> IORef.IORef Double -> IO [SampleGroup]
collectCounter info c = do
    value <- IORef.readIORef c
    let sample = Sample (metricName info) [] (BS.fromString $ show value)
    return [SampleGroup info CounterType [sample]]

-- | Count the amount of times an action throws any synchronous exception.
--
-- >>> exceptions <- register $ counter (Info "exceptions_total" "Total amount of exceptions thrown")
-- >>> countExceptions exceptions $ return ()
-- >>> getCounter exceptions
-- 0.0
-- >>> countExceptions exceptions (error "Oh no!") `catch` (\SomeException{} -> return ())
-- >>> getCounter exceptions
-- 1.0
--
-- It's important to note that this will count *all* synchronous exceptions. If
-- you want more granular counting of exceptions, you will need to write custom
-- code using 'incCounter'.
countExceptions :: (MonadCatch m, MonadMonitor m) => Counter -> m a -> m a
countExceptions m io = io `onException` incCounter m

-- $setup
-- >>> :module +Prometheus
-- >>> :set -XOverloadedStrings
