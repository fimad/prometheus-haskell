{-# language OverloadedStrings #-}
{-# language CPP #-}

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
) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.UTF8 as BS
import Data.Text (Text)
#if __GLASGOW_HASKELL__ < 804
import GHC.Conc (numSparks, getNumCapabilities)
#endif
import GHC.Stats
import Prometheus


data GHCMetrics = GHCMetrics

ghcMetrics :: Metric GHCMetrics
ghcMetrics = Metric (return (GHCMetrics, concat <$> sequence ghcCollectors))

#if __GLASGOW_HASKELL__ < 804
ghcCollectors :: [IO [SampleGroup]]
ghcCollectors = [
        showCollector
            "ghc_sparks"
            "The number of sparks in the local spark pool."
            GaugeType
            numSparks
    ,   showCollector
            "ghc_capabilities"
            "The number of threads that can run truly simultaneously."
            GaugeType
            getNumCapabilities
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
ghcCollectors :: [IO [SampleGroup]]
ghcCollectors = [
      statsCollector "gcs" "Total number of GCs" CounterType gcs
    , statsCollector "major_gcs" "Total number of major (oldest generation) GCs" CounterType major_gcs
    , statsCollector "allocated_bytes" "Total bytes allocated" CounterType allocated_bytes
    , statsCollector "max_live_bytes" "Maximum live data (including large objects + compact regions)" CounterType max_live_bytes
    , statsCollector "max_large_objects_bytes" "Maximum live data in large objects" CounterType max_large_objects_bytes
    , statsCollector "max_compact_bytes" "Maximum live data in compact regions" CounterType max_compact_bytes
    , statsCollector "max_slop_bytes" "Maximum slop" CounterType max_slop_bytes
    , statsCollector "max_mem_in_use_bytes" "Maximum memory in use by the RTS" CounterType max_mem_in_use_bytes
    , statsCollector "cumulative_live_bytes" "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program." CounterType cumulative_live_bytes
    , statsCollector "copied_bytes" "Sum of copied_bytes across all GCs" CounterType copied_bytes
    , statsCollector "par_copied_bytes" "Sum of copied_bytes across all parallel GCs" CounterType par_copied_bytes
    , statsCollector "cumulative_par_max_copied_bytes" "Sum of par_max_copied_bytes across all parallel GCs" CounterType cumulative_par_max_copied_bytes
    , statsCollector "mutator_cpu_ns" "Total CPU time used by the mutator" CounterType mutator_cpu_ns
    , statsCollector "mutator_elapsed_ns" "Total elapsed time used by the mutator" CounterType mutator_elapsed_ns
    , statsCollector "gc_cpu_ns" "Total CPU time used by the GC" CounterType gc_cpu_ns
    , statsCollector "gc_elapsed_ns" "Total elapsed time used by the GC" CounterType gc_elapsed_ns
    , statsCollector "cpu_ns" "Total CPU time (at the previous GC)" CounterType cpu_ns
    , statsCollector "elapsed_ns" "Total elapsed time (at the previous GC)" CounterType elapsed_ns

    , statsCollector "gcdetails_gen" "The generation number of this GC" CounterType (gcdetails_gen . gc)
    , statsCollector "gcdetails_threads" "Number of threads used in this GC" CounterType (gcdetails_threads . gc)
    , statsCollector "gcdetails_allocated_bytes" "Number of bytes allocated since the previous GC" CounterType (gcdetails_allocated_bytes . gc)
    , statsCollector "gcdetails_live_bytes" "Total amount of live data in the heap (incliudes large + compact data)" CounterType (gcdetails_live_bytes . gc)
    , statsCollector "gcdetails_large_objects_bytes" "Total amount of live data in large objects" CounterType (gcdetails_large_objects_bytes . gc)
    , statsCollector "gcdetails_compact_bytes" "Total amount of live data in compact regions" CounterType (gcdetails_compact_bytes . gc)
    , statsCollector "gcdetails_slop_bytes" "Total amount of slop (wasted memory)" CounterType (gcdetails_slop_bytes . gc)
    , statsCollector "gcdetails_mem_in_use_bytes" "Total amount of memory in use by the RTS" CounterType (gcdetails_mem_in_use_bytes . gc)
    , statsCollector "gcdetails_copied_bytes" "Total amount of data copied during this GC" CounterType (gcdetails_copied_bytes . gc)
    , statsCollector "gcdetails_par_max_copied_bytes" "In parallel GC, the max amount of data copied by any one thread" CounterType (gcdetails_par_max_copied_bytes . gc)
    , statsCollector "gcdetails_sync_elapsed_ns" "The time elapsed during synchronisation before GC" CounterType (gcdetails_sync_elapsed_ns . gc)
    , statsCollector "gcdetails_cpu_ns" "The CPU time used during GC itself" CounterType (gcdetails_cpu_ns . gc)
    , statsCollector "gcdetails_elapsed_ns" "The time elapsed during GC itself" CounterType (gcdetails_elapsed_ns . gc)
  ]
#endif

#if __GLASGOW_HASKELL__ < 804
statsCollector :: Show a
               => Text -> Text -> SampleType -> (GCStats -> a) -> IO [SampleGroup]
statsCollector name help sampleType stat = do
    statsEnabled <- getGCStatsEnabled
    if statsEnabled
        then showCollector name help sampleType (stat <$> getGCStats)
        else return []
#else
statsCollector :: Show a
               => Text -> Text -> SampleType -> (RTSStats -> a) -> IO [SampleGroup]
statsCollector name help sampleType stat = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled
        then showCollector name help sampleType (stat <$> getRTSStats)
        else return []
#endif

showCollector :: Show a => Text -> Text -> SampleType -> IO a -> IO [SampleGroup]
showCollector name help sampleType ioInt = do
    value <- ioInt
    let info = Info name help
    let valueBS = BS.fromString $ show value
    return [SampleGroup info sampleType [Sample name [] valueBS]]
