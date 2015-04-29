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
            numSparks
    ,   showCollector
            "ghc_capabilities"
            "The number of threads that can run truly simultaneously."
            getNumCapabilities
    ,   statsCollector
            "ghc_bytes_allocated"
            "Total number of bytes allocated."
            bytesAllocated
    ,   statsCollector
            "ghc_num_gcs"
            "The number of garbage collections performed."
            numGcs
    ,   statsCollector
            "ghc_max_bytes_used"
            "The maximum number of live bytes seen so far."
            maxBytesUsed
    ,   statsCollector
            "ghc_cumulative_bytes_used"
            "The cumulative total bytes used."
            cumulativeBytesUsed
    ,   statsCollector
            "ghc_bytes_copied"
            "The number of bytes copied during garbage collection."
            bytesCopied
    ,   statsCollector
            "ghc_current_bytes_used"
            "The number of current live bytes."
            currentBytesUsed
    ,   statsCollector
            "ghc_current_bytes_slop"
            "The current number of bytes lost to slop."
            currentBytesSlop
    ,   statsCollector
            "ghc_max_bytes_slop"
            "The maximum number of bytes lost to slop so far."
            maxBytesSlop
    ,   statsCollector
            "ghc_peak_megabytes_allocated"
            "The maximum number of megabytes allocated."
            peakMegabytesAllocated
    ,   statsCollector
            "ghc_mutator_cpu_seconds"
            "The CPU time spent running mutator threads."
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_mutator_wall_seconds"
            "The wall clock time spent running mutator threads."
            mutatorCpuSeconds
    ,   statsCollector
            "ghc_gc_cpu_seconds"
            "The CPU time spent running GC."
            gcCpuSeconds
    ,   statsCollector
            "ghc_gc_wall_seconds"
            "The wall clock time spent running GC."
            gcWallSeconds
    ,   statsCollector
            "ghc_cpu_seconds"
            "Total CPU time elapsed since program start."
            cpuSeconds
    ,   statsCollector
            "ghc_wall_seconds"
            "Total wall clock time elapsed since start."
            wallSeconds
    ,   statsCollector
            "ghc_parallel_total_bytes_copied"
            ("Number of bytes copied during GC, minus space held by mutable "
                ++ "lists held by the capabilities.")
            parTotBytesCopied
    ,   statsCollector
            "ghc_parallel_max_bytes_copied"
            ("Sum of number of bytes copied each GC by the most active GC "
                ++ "thread each GC.")
            parMaxBytesCopied
    ]

statsCollector :: Show a
               => String -> String -> (GCStats -> a) -> IO [SampleGroup]
statsCollector name help stat = do
    statsEnabled <- getGCStatsEnabled
    if statsEnabled
        then showCollector name help (stat <$> getGCStats)
        else return []

showCollector :: Show a => String -> String -> IO a -> IO [SampleGroup]
showCollector name help ioInt = do
    value <- ioInt
    let info = Info name help
    let valueBS = BS.fromString $ show value
    return [SampleGroup info GaugeType [Sample name [] valueBS]]
