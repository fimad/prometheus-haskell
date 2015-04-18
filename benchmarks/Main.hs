import Prometheus

import Control.Monad
import Criterion.Main
import System.Random
import qualified Data.ByteString as BS


main :: IO ()
main = defaultMain [
        bgroup "incCounter" $ expandBenches incCounterThenCollect
    ,   bgroup "addGauge" $ expandBenches $ withGaugeThenCollect (addGauge 47.0)
    ,   bgroup "subGauge" $ expandBenches $ withGaugeThenCollect (subGauge 47.0)
    ,   bgroup "setGauge" $ expandBenches $ withGaugeThenCollect (setGauge 47.0)
    ,   bgroup "observe" $ expandBenches observeThenCollect
    ]

expandBenches :: (Int -> IO a) -> [Benchmark]
expandBenches b = expand b [1, 10, 100, 1000, 10000]
    where
        expand b = map (\i -> bench (show i) (whnfIO (b i)))

counterBGroup :: Benchmark
counterBGroup = bgroup "incCounter" $ expandBenches incCounterThenCollect

incCounterThenCollect :: Int -> IO [SampleGroup]
incCounterThenCollect i = do
    c <- counter (Info "name" "help")
    replicateM_ i (incCounter c)
    collect c

withGaugeThenCollect :: (Metric Gauge -> IO ()) -> Int -> IO [SampleGroup]
withGaugeThenCollect a i = do
    g <- gauge (Info "name" "help")
    replicateM_ i (a g)
    collect g

observeThenCollect :: Int -> IO [SampleGroup]
observeThenCollect i = do
    s <- summary (Info "name" "help") defaultQuantiles
    replicateM_ i (randomIO >>= flip observe s)
    collect s
