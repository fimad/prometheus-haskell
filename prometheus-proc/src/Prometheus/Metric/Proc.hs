{-# language ForeignFunctionInterface #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Prometheus.Metric.Proc ( procMetrics ) where

import Data.Char ( isSpace )
import Data.Maybe ( catMaybes )
import Data.String ( fromString )
import Foreign.C
import Prometheus
import System.FilePath
import System.IO.Unsafe
import System.Posix.Memory ( sysconfPageSize )
import System.Posix.Process ( getProcessID )
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Common as RE


data ProcMetrics =
  ProcMetrics


procMetrics :: Prometheus.Metric ProcMetrics
procMetrics =
  Metric ( return ( ProcMetrics, collect ) )


foreign import ccall unsafe
  clk_tck :: CLong


collect :: IO [ SampleGroup ]
collect = do
  pid <-
    getProcessID

  mprocStat <-
      RE.match parseProcStat <$> readFile ( "/" </> "proc" </> show pid </> "stat" )

  return
    ( foldMap ( toMetrics ) mprocStat )


toMetrics :: ProcStat -> [ SampleGroup ]
toMetrics ProcStat{..} =
  catMaybes
    [ Just process_cpu_seconds_total
    , process_start_time_seconds
    , Just process_virtual_memory_bytes
    , Just process_resident_memory_bytes
    ]

  where

    process_cpu_seconds_total =
      metric
        "process_cpu_seconds_total"
        "Total user and system CPU time spent in seconds."
        CounterType
        ( fromTicks ( utime + stime ) )

    process_start_time_seconds = do
      btime <-
        mbtime

      return
        ( metric
            "process_start_time_seconds"
            "Start time of the process since unix epoch in seconds."
            GaugeType
            ( fromIntegral btime + fromTicks starttime )
        )

    process_virtual_memory_bytes =
      metric
        "process_virtual_memory_bytes"
        "Virtual memory size in bytes."
        GaugeType
        vsize

    process_resident_memory_bytes =
      metric
        "process_resident_memory_bytes"
        "Resident memory size in bytes."
        GaugeType
        ( rss * sysconfPageSize )

    fromTicks ticks =
      fromIntegral ticks / fromIntegral clk_tck :: Double

    metric metricName metricHelp metricType value =
      SampleGroup
        Info{..}
        metricType
        [ Sample
            metricName
            []
            ( fromString ( show value ) )
        ]


{-# NOINLINE mbtime #-}
mbtime :: Maybe Int
mbtime = unsafePerformIO $ do
  fmap ( \( _, a, _ ) -> a ) . RE.findFirstInfix ( "btime " *> RE.decimal )
    <$> readFile "/proc/stat"


data ProcStat = ProcStat
  { utime :: Int
  , stime :: Int
  , starttime :: Int
  , vsize :: Int
  , rss :: Int
  }
  deriving
    ( Show )


parseProcStat :: RE.RE Char ProcStat
parseProcStat =
  ProcStat
    <$  any                                                     -- pid %d
    <*  token ( RE.sym '(' *> RE.some RE.anySym <* RE.sym ')' ) -- comm %s
    <*  any                                                     -- state %c
    <*  any                                                     -- ppid %d
    <*  any                                                     -- pgrp %d
    <*  any                                                     -- session %d
    <*  any                                                     -- tty_nr %d
    <*  any                                                     -- tpgid %d
    <*  any                                                     -- flags %u (%lu before Linux 2.6.22)
    <*  any                                                     -- minflt %lu
    <*  any                                                     -- cminflt %lu
    <*  any                                                     -- majflt %lu
    <*  any                                                     -- cmajflt %lu
    <*> token RE.decimal                                        -- utime %lu
    <*> token RE.decimal                                        -- stime %lu
    <*  any                                                     -- cutime %ld
    <*  any                                                     -- cstime %ld
    <*  any                                                     -- priority %ld
    <*  any                                                     -- nice %ld
    <*  any                                                     -- num_threads %ld
    <*  any                                                     -- itrealvalue %ld
    <*> token RE.decimal                                        -- starttime %llu (was %lu before Linux 2.6)
    <*> token RE.decimal                                        -- vsize %lu
    <*> token RE.decimal                                        -- rss %ld
    <*  any                                                     -- rsslim %lu
    <*  any                                                     -- startcode %lu
    <*  any                                                     -- endcode %lu
    <*  any                                                     -- startstack %lu
    <*  any                                                     -- kstkesp %lu
    <*  any                                                     -- kstkeip %lu
    <*  any                                                     -- signal %lu
    <*  any                                                     -- blocked %lu
    <*  any                                                     -- sigignore %lu
    <*  any                                                     -- sigcatch %lu
    <*  any                                                     -- wchan %lu
    <*  any                                                     -- nswap %lu
    <*  any                                                     -- cnswap %lu
    <*  any                                                     -- exit_signal %d (since Linux 2.1.22)
    <*  any                                                     -- processor %d (since Linux 2.2.8)
    <*  any                                                     -- rt_priority %u (since Linux 2.5.19; was %lu before Linux 2.6.22)
    <*  any                                                     -- policy %u (since Linux 2.5.19; was %lu before Linux 2.6.22)
    <*  any                                                     -- delayacct_blkio_ticks %llu (since Linux 2.6.18)
    <*  any                                                     -- guest_time %lu (since Linux 2.6.24)
    <*  any                                                     -- cguest_time %ld (since Linux 2.6.24)

  where

    token a =
      a <* RE.psym isSpace <* RE.few ( RE.psym isSpace )

    any =
      token ( RE.few RE.anySym )
