module Prometheus.Metric.Observer (
    Observer(..)
,   observeDuration
,   timeAction
) where

import Data.Ratio ((%))
import Prometheus.MonadMonitor

import Control.Monad.IO.Class
import System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)

-- | Interface shared by 'Summary' and 'Histogram'.
class Observer metric where
    -- | Observe that a particular floating point value has occurred.
    -- For example, observe that this request took 0.23s.
    observe :: MonadMonitor m => Double -> metric -> m ()

-- | Adds the duration in seconds of an IO action as an observation to an
-- observer metric.
--
-- If the IO action throws an exception no duration will be observed.
observeDuration :: (Observer metric, MonadIO m, MonadMonitor m) => m a -> metric -> m a
observeDuration io metric = do
    (result, duration) <- timeAction io
    observe duration metric
    return result


-- | Evaluate @io@ and return its result as well as how long it took to evaluate,
-- in seconds.
timeAction :: MonadIO m => m a -> m (a, Double)
timeAction io = do
    start  <- liftIO $ getTime Monotonic
    result <- io
    end    <- liftIO $ getTime Monotonic
    let duration = toNanoSecs (end `diffTimeSpec` start) % 1000000000
    return (result, fromRational duration)
