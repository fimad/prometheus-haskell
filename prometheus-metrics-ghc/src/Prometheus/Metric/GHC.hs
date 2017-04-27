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
import GHC.Conc (numSparks, getNumCapabilities)
import GHC.Stats
import Prometheus
import qualified Data.ByteString.UTF8 as BS


newtype GHCMetrics = GHCMetrics ()

ghcMetrics :: Metric GHCMetrics
ghcMetrics = Metric {
        handle  = GHCMetrics ()
    ,   collect = concat <$> sequence ghcCollectors
    }

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
            "ghc_bytes_allocated"
            "Total number of bytes allocated."
            CounterType
            bytesAllocated
    ,   statsCollector
            "ghc_num_gcs"
            "The number of garbage collections performed."
            CounterType
            numGcs
    ,   statsCollector
            "ghc_max_bytes_used"
            "The maximum number of live bytes seen so far."
            GaugeType
            maxBytesUsed
    ,   statsCollector
            "ghc_cumulative_bytes_used"
            "The cumulative total bytes used."
            CounterType
            cumulativeBytesUsed
    ,   statsCollector
            "ghc_bytes_copied"
            "The number of bytes copied during garbage collection."
            CounterType
            bytesCopied
    ,   statsCollector
            "ghc_current_bytes_used"
            "The number of current live bytes."
            GaugeType
            currentBytesUsed
    ,   statsCollector
            "ghc_current_bytes_slop"
            "The current number of bytes lost to slop."
            GaugeType
            currentBytesSlop
    ,   statsCollector
            "ghc_max_bytes_slop"
            "The maximum number of bytes lost to slop so far."
            GaugeType
            maxBytesSlop
    ,   statsCollector
            "ghc_peak_megabytes_allocated"
            "The maximum number of megabytes allocated."
            GaugeType
            peakMegabytesAllocated
    ,   statsCollector
            "ghc_mutator_cpu_seconds"
            "The CPU time spent running mutator threads."
            CounterType
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_mutator_wall_seconds"
            "The wall clock time spent running mutator threads."
            CounterType
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_gc_cpu_seconds"
            "The CPU time spent running GC."
            CounterType
            gcCpuSeconds
    ,   statsCollector
            "ghc_gc_wall_seconds"
            "The wall clock time spent running GC."
            CounterType
            gcWallSeconds
    ,   statsCollector
            "ghc_cpu_seconds"
            "Total CPU time elapsed since program start."
            CounterType
            cpuSeconds
    ,   statsCollector
            "ghc_wall_seconds"
            "Total wall clock time elapsed since start."
            CounterType
            wallSeconds
    ,   statsCollector
            "ghc_parallel_total_bytes_copied"
            ("Number of bytes copied during GC, minus space held by mutable "
                ++ "lists held by the capabilities.")
            CounterType
            parTotBytesCopied
    ,   statsCollector
            "ghc_parallel_max_bytes_copied"
            ("Sum of number of bytes copied each GC by the most active GC "
                ++ "thread each GC.")
            CounterType
            parMaxBytesCopied
    ]

statsCollector :: Show a
               => String -> String -> SampleType -> (GCStats -> a) -> IO [SampleGroup]
statsCollector name help sampleType stat = do
    statsEnabled <- getGCStatsEnabled
    if statsEnabled
        then showCollector name help sampleType (stat <$> getGCStats)
        else return []

showCollector :: Show a => String -> String -> SampleType -> IO a -> IO [SampleGroup]
showCollector name help sampleType ioInt = do
    value <- ioInt
    let info = Info name help
    let valueBS = BS.fromString $ show value
    return [SampleGroup info GaugeType [Sample name [] valueBS]]
