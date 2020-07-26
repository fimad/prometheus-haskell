{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language NumDecimals #-}

-- | This module defines a metrics that exposes statistics from the GHC runtime
-- system ("GHC.Conc", "GHC.Stats").
--
-- To use these metrics, the monitored executable should run with the `+RTS -T`
-- command line flags and the following must be added somewhere near the
-- beginning of the main method:
--
-- >>> register ghcMetrics
module Prometheus.Metric.GHC (
    GHCMetrics
,   ghcMetrics
,   ghcMetricsWithLabels
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import qualified Data.ByteString.UTF8 as BS
import Data.Text (Text)
import Data.Fixed (Fixed, E9)
#if __GLASGOW_HASKELL__ < 804
import GHC.Conc (numSparks, getNumCapabilities)
import GHC.Stats (GCStats(..), getGCStatsEnabled, getGCStats)
#else
import GHC.Stats (RTSStats(..), GCDetails(..), getRTSStatsEnabled, getRTSStats)
#endif
import qualified GHC.Stats as Stats
import Prometheus


data GHCMetrics = GHCMetrics

ghcMetrics :: Metric GHCMetrics
ghcMetrics = ghcMetricsWithLabels []

ghcMetricsWithLabels :: LabelPairs -> Metric GHCMetrics
ghcMetricsWithLabels labels = Metric (do
  statsEnabled <-
#if __GLASGOW_HASKELL__ < 804
    getGCStatsEnabled
#else
    getRTSStatsEnabled
#endif
  if statsEnabled
  then return (GHCMetrics, do
        stats <-
#if __GLASGOW_HASKELL__ < 804
            getGCStats
#else
            getRTSStats
#endif
        concat <$> mapM (\f -> f labels stats) ghcCollectors
    )
  else return (GHCMetrics, return [])
  )

#if __GLASGOW_HASKELL__ < 804
ghcCollectors :: [LabelPairs -> GCStats -> IO [SampleGroup]]
ghcCollectors = [
        \labelpairs gcstats -> do
          sparkCount <- numSparks
          showCollector
            "ghc_sparks"
            "The number of sparks in the local spark pool."
            GaugeType
            sparkCount
            labelpairs
    ,   \labelpairs gcstats -> do
          numCapabilities <- getNumCapabilities
          showCollector
            "ghc_capabilities"
            "The number of threads that can run truly simultaneously."
            GaugeType
            numCapabilities
            labelpairs
    ,   statsCollector
            "ghc_allocated_bytes_total"
            "Total number of bytes allocated."
            CounterType
            bytesAllocated
    ,   statsCollector
            "ghc_num_gcs"
            "The number of garbage collections performed."
            CounterType
            numGcs
    ,   statsCollector
            "ghc_max_used_bytes"
            "The maximum number of live bytes seen so far."
            GaugeType
            maxBytesUsed
    ,   statsCollector
            "ghc_cumulative_used_bytes_total"
            "The cumulative total bytes used."
            CounterType
            cumulativeBytesUsed
    ,   statsCollector
            "ghc_copied_bytes_total"
            "The number of bytes copied during garbage collection."
            CounterType
            bytesCopied
    ,   statsCollector
            "ghc_current_used_bytes"
            "The number of current live bytes."
            GaugeType
            currentBytesUsed
    ,   statsCollector
            "ghc_current_slop_bytes"
            "The current number of bytes lost to slop."
            GaugeType
            currentBytesSlop
    ,   statsCollector
            "ghc_max_slop_bytes"
            "The maximum number of bytes lost to slop so far."
            GaugeType
            maxBytesSlop
    ,   statsCollector
            "ghc_peak_allocated_megabytes" -- XXX: export as bytes?
            "The maximum number of megabytes allocated."
            GaugeType
            peakMegabytesAllocated
    ,   statsCollector
            "ghc_mutator_cpu_seconds_total"
            "The CPU time spent running mutator threads."
            CounterType
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_mutator_wall_seconds_total"
            "The wall clock time spent running mutator threads."
            CounterType
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_gc_cpu_seconds_total"
            "The CPU time spent running GC."
            CounterType
            gcCpuSeconds
    ,   statsCollector
            "ghc_gc_wall_seconds_total"
            "The wall clock time spent running GC."
            CounterType
            gcWallSeconds
    ,   statsCollector
            "ghc_cpu_seconds_total"
            "Total CPU time elapsed since program start."
            CounterType
            cpuSeconds
    ,   statsCollector
            "ghc_wall_seconds_total"
            "Total wall clock time elapsed since start."
            CounterType
            wallSeconds
    ,   statsCollector
            "ghc_parallel_copied_bytes_total"
            "Number of bytes copied during GC, minus space held by mutable lists held by the capabilities."
            CounterType
            parTotBytesCopied
    ,   statsCollector
            "ghc_parallel_max_copied_bytes_total"
            "Sum of number of bytes copied each GC by the most active GC thread each GC."
            CounterType
            parMaxBytesCopied
    ]

#else

ghcCollectors :: [LabelPairs -> RTSStats -> IO [SampleGroup]]
ghcCollectors = [
      statsCollector
            "ghc_gcs_total"
            "Total number of GCs"
            CounterType
            gcs
    , statsCollector
            "ghc_major_gcs_total"
            "Total number of major (oldest generation) GCs"
            CounterType
            major_gcs
    , statsCollector
            "ghc_allocated_bytes_total"
            "Total bytes allocated"
            CounterType
            allocated_bytes
    , statsCollector
            "ghc_max_live_bytes"
            "Maximum live data (including large objects + compact regions)"
            GaugeType
            max_live_bytes
    , statsCollector
            "ghc_max_large_objects_bytes"
            "Maximum live data in large objects"
            GaugeType
            max_large_objects_bytes
    , statsCollector
            "ghc_max_compact_bytes"
            "Maximum live data in compact regions"
            GaugeType
            max_compact_bytes
    , statsCollector
            "ghc_max_slop_bytes"
            "Maximum slop"
            GaugeType
            max_slop_bytes
    , statsCollector
            "ghc_max_mem_in_use_bytes"
            "Maximum memory in use by the RTS"
            GaugeType
            max_mem_in_use_bytes
    , statsCollector
            "ghc_cumulative_live_bytes_total"
            "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program."
            CounterType
            cumulative_live_bytes
    , statsCollector
            "ghc_copied_bytes_total"
            "Sum of copied_bytes across all GCs"
            CounterType
            copied_bytes
    , statsCollector
            "ghc_par_copied_bytes_total"
            "Sum of copied_bytes across all parallel GCs"
            CounterType
            par_copied_bytes
    , statsCollector
            "ghc_cumulative_par_max_copied_bytes_total"
            "Sum of par_max_copied_bytes across all parallel GCs"
            CounterType
            cumulative_par_max_copied_bytes
    , statsCollector
            "ghc_mutator_cpu_seconds_total"
            "Total CPU time used by the mutator"
            CounterType
            (rtsTimeToSeconds . mutator_cpu_ns)
    , statsCollector
            "ghc_mutator_elapsed_seconds_total"
            "Total elapsed time used by the mutator"
            CounterType
            (rtsTimeToSeconds . mutator_elapsed_ns)
    , statsCollector
            "ghc_gc_cpu_seconds_total"
            "Total CPU time used by the GC"
            CounterType
            (rtsTimeToSeconds . gc_cpu_ns)
    , statsCollector
            "ghc_gc_elapsed_seconds_total"
            "Total elapsed time used by the GC"
            CounterType
            (rtsTimeToSeconds . gc_elapsed_ns)
    , statsCollector
            "ghc_cpu_seconds_total"
            "Total CPU time (at the previous GC)"
            CounterType
            (rtsTimeToSeconds . cpu_ns)
    , statsCollector
            "ghc_elapsed_seconds_total"
            "Total elapsed time (at the previous GC)"
            CounterType
            (rtsTimeToSeconds . elapsed_ns)

    , statsCollector
            "ghc_gcdetails_gen"
            "The generation number of this GC"
            HistogramType -- TODO: is this correct?
                          -- Gauge makes little sense here.
                          -- With Histogram we'll be able to see which
                          -- generations are collected most often.
            (gcdetails_gen . gc)
    , statsCollector
            "ghc_gcdetails_threads"
            "Number of threads used in this GC"
            GaugeType
            (gcdetails_threads . gc)
    , statsCollector
            "ghc_gcdetails_allocated_bytes"
            "Number of bytes allocated since the previous GC"
            GaugeType -- TODO: this doesn't seem very meaningful.
            (gcdetails_allocated_bytes . gc)
    , statsCollector
            "ghc_gcdetails_live_bytes"
            "Total amount of live data in the heap (including large + compact data)"
            GaugeType
            (gcdetails_live_bytes . gc)
    , statsCollector
            "ghc_gcdetails_large_objects_bytes"
            "Total amount of live data in large objects"
            GaugeType
            (gcdetails_large_objects_bytes . gc)
    , statsCollector
            "ghc_gcdetails_compact_bytes"
            "Total amount of live data in compact regions"
            GaugeType
            (gcdetails_compact_bytes . gc)
    , statsCollector
            "ghc_gcdetails_slop_bytes"
            "Total amount of slop (wasted memory)"
            GaugeType
            (gcdetails_slop_bytes . gc)
    , statsCollector
            "ghc_gcdetails_mem_in_use_bytes"
            "Total amount of memory in use by the RTS"
            CounterType
            (gcdetails_mem_in_use_bytes . gc)
    , statsCollector
            "ghc_gcdetails_copied_bytes"
            "Total amount of data copied during this GC"
            GaugeType -- TODO: this will also vary wildly between GCs of different generations.
            (gcdetails_copied_bytes . gc)
    , statsCollector
            "ghc_gcdetails_par_max_copied_bytes"
            "In parallel GC, the max amount of data copied by any one thread"
            GaugeType
            (gcdetails_par_max_copied_bytes . gc)
    , statsCollector
            "ghc_gcdetails_sync_elapsed_seconds"
            "The time elapsed during synchronisation before GC"
            GaugeType
            (rtsTimeToSeconds . gcdetails_sync_elapsed_ns . gc)
    , statsCollector
            "ghc_gcdetails_cpu_seconds"
            "The CPU time used during GC itself"
            GaugeType
            (rtsTimeToSeconds . gcdetails_cpu_ns . gc)
    , statsCollector
            "ghc_gcdetails_elapsed_seconds"
            "The time elapsed during GC itself"
            GaugeType
            (rtsTimeToSeconds . gcdetails_elapsed_ns . gc)
  ]

-- | Convert from 'RtsTime' (nanoseconds) to seconds with nanosecond precision.
rtsTimeToSeconds :: Stats.RtsTime -> Fixed E9
rtsTimeToSeconds = (/ 1e9) . fromIntegral
#endif

#if __GLASGOW_HASKELL__ < 804
statsCollector :: Show a
               => Text -> Text -> SampleType -> (GCStats -> a) -> LabelPairs -> GCStats -> IO [SampleGroup]
statsCollector name help sampleType stat labels gcstats =
    showCollector name help sampleType (stat gcstats) labels
#else
statsCollector :: Show a
               => Text -> Text -> SampleType -> (RTSStats -> a) -> LabelPairs -> RTSStats -> IO [SampleGroup]
statsCollector name help sampleType stat labels rtsStats =
    showCollector name help sampleType (stat rtsStats) labels
#endif

showCollector :: Show a => Text -> Text -> SampleType -> a -> LabelPairs -> IO [SampleGroup]
showCollector name help sampleType value labels = do
    let info = Info name help
    let valueBS = BS.fromString $ show value
    return [SampleGroup info sampleType [Sample name labels valueBS]]
