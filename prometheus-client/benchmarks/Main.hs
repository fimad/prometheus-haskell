{-# language FlexibleContexts #-}

module Main (main) where

import Prometheus

import Control.Monad
import Criterion
import Criterion.Main
import Data.Foldable (for_)
import System.Random

withMetric m =
  envWithCleanup
    (register m)
    (const unregisterAll)

withCounter =
  withMetric (counter (Info "a" "b"))

withGauge =
  withMetric (gauge (Info "a" "b"))

withSummary quantiles =
  withMetric (summary (Info "a" "b") quantiles)

withHistogram buckets =
  withMetric (histogram (Info "a" "b") buckets)

main :: IO ()
main =
  defaultMain
    [ benchCounter
    , benchGauge
    , benchSummary
    , benchHistogram
    , benchExport
    ]


-- Counter benchmarks


benchCounter =
  withCounter $ \counter ->
    bgroup "Counter"
      [ benchIncCounter counter
      , benchAddCounter counter
      , benchAddDurationToCounter counter
      ]

benchIncCounter testCounter =
  bench "incCounter" $ whnfIO (incCounter testCounter)

benchAddCounter testCounter =
  bench "addCounter" $ whnfIO (addCounter 50 testCounter)

benchAddDurationToCounter testCounter =
  bench "addDurationToCounter" $ whnfIO (addDurationToCounter (return ()) testCounter)



-- Gauge benchmarks


benchGauge =
  withGauge $ \gauge ->
    bgroup "Gauge"
      [ benchIncGauge gauge
      , benchAddGauge gauge
      , benchSubGauge gauge
      , benchSetGaugeToDuration gauge
      ]

benchIncGauge testGauge =
  bench "incGauge" $ whnfIO (incGauge testGauge)

benchAddGauge testGauge =
  bench "addGauge" $ whnfIO (addGauge 50 testGauge)

benchSubGauge testGauge =
  bench "subGauge" $ whnfIO (subGauge 50 testGauge)

benchSetGaugeToDuration testGauge =
  bench "setGaugeToDuration" $ whnfIO (setGaugeToDuration (return ()) testGauge)



-- Summary benchmarks


benchSummary =
  bgroup "Summary"
    (map benchSummaryWithQuantiles [defaultQuantiles])

benchSummaryWithQuantiles q =
  withSummary q $ \summary ->
    bgroup ("Quantiles = " ++ show q)
      [ benchSummaryObserve summary
      ]

benchSummaryObserve s =
  bench "observe" $ whnfIO (observe 42 s)



-- Histogram benchmarks


benchHistogram =
  bgroup "Histogram"
    (map benchHistogramWithQuantiles [defaultBuckets])

benchHistogramWithQuantiles q =
  withHistogram q $ \histogram ->
    bgroup ("Buckets = " ++ show q)
      [ benchHistogramObserve histogram
      ]

benchHistogramObserve s =
  bench "observe" $ whnfIO (observe 42 s)



-- Exporter benchmarks

benchExport =
  bgroup "exportMetricsAsText"
    [ bgroup "Export counters" (map benchExportCounters [100, 1000, 10000])
    , bgroup "Export histograms" (map benchExportHistograms [100, 1000, 10000])
    ]

benchExportCounters nCounters =
  envWithCleanup setup teardown ( const benchmark )
  where
    benchmark = bench (show nCounters ++ " counters") (nfIO exportMetricsAsText)
    setup = replicateM_ nCounters $ do
      register $ counter (Info (show nCounters) "")
    teardown _ = unregisterAll

benchExportHistograms nHistograms =
  envWithCleanup setup teardown ( const benchmark )
  where
    benchmark = bench (show nHistograms ++ " histograms") (nfIO exportMetricsAsText)
    setup = replicateM_ nHistograms $ do
      register $ histogram (Info (show nHistograms) "") defaultBuckets
    teardown _ = unregisterAll
