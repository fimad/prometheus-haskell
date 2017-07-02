module Prometheus.Metric.Observer (
    Observer(..)
,   observeDuration
,   timeAction
) where

import Prometheus.Metric
import Prometheus.MonadMonitor

import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- | Interface shared by 'Summary' and 'Histogram'.
class Observer metric where
    -- | Observe that a particular floating point value has occurred.
    -- For example, observe that this request took 0.23s.
    observe :: MonadMonitor m => Double -> Metric metric -> m ()

-- | Adds the duration in seconds of an IO action as an observation to an
-- observer metric.
observeDuration :: Observer metric => IO a -> Metric metric -> IO a
observeDuration io metric = do
    (result, duration) <- timeAction io
    observe duration metric
    return result


-- | Evaluate @io@ and return its result as well as how long it took to evaluate,
-- in seconds.
timeAction :: IO a -> IO (a, Double)
timeAction io = do
    start  <- getCurrentTime
    result <- io
    end    <- getCurrentTime
    return (result, fromRational $ toRational $ end `diffUTCTime` start)
