{-# language ForeignFunctionInterface #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

{-|

This module exposes a @prometheus-client@ "Metric" for exporting information
about the currently running process.

-}
module Prometheus.Metric.Proc ( ProcMetrics(..), procMetrics ) where

import Data.Char ( isSpace )
import Data.Int ( Int64 )
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


-- | The tag for 'procMetrics'.
data ProcMetrics =
  ProcMetrics


{-|

Unregistered metrics for the current process. This is to be used with
'Prometheus.register' to register the metrics.

This exports the following:


* @process_cpu_seconds_total@
* @process_start_time_seconds@
* @process_virtual_memory_bytes@
* @process_resident_memory_bytes@

See the official Prometheus documentation for more information on these standard
metrics: https://prometheus.io/docs/instrumenting/writing_clientlibs/#standard-and-runtime-collectors

-}
procMetrics :: Prometheus.Metric ProcMetrics
procMetrics =
  Metric ( return ( ProcMetrics, collect ) )


-- | Returns the number of CPU clock ticks per second.
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
toMetrics ProcStat{ utime, stime, starttime, vsize, rss } =
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
        ( rss * fromIntegral sysconfPageSize )

    metric metricName metricHelp metricType value =
      SampleGroup
        Info{..}
        metricType
        [ Sample
            metricName
            []
            ( fromString ( show value ) )
        ]


-- | Convert a number of clock ticks into the corresponding duration in seconds.
fromTicks :: Int64 -> Double
fromTicks ticks =
  fromIntegral ticks / fromIntegral clk_tck


{-|

Returns the current boot time in seconds since Unix epoch. This is a Maybe
as we might not to be able to successfully parse this information out of
@/proc/stat@.

'unsafePerformIO' is used as this value does not change during the
execution of the program, so this gives us a lightweight cache for this
value.

-}
{-# NOINLINE mbtime #-}
mbtime :: Maybe Int64
mbtime = unsafePerformIO $ do
  fmap ( \( _, a, _ ) -> a ) . RE.findFirstInfix ( "btime " *> RE.decimal )
    <$> readFile "/proc/stat"


-- | Specific metrics from @/proc/xyz/stat@ that we are interested in.
data ProcStat = ProcStat
  { utime :: Int64
    -- ^ Amount of time that this process has been scheduled in user mode,
    -- measured in clock ticks (divide by sysconf(_SC_CLK_TCK)).
  , stime :: Int64
    -- ^ Amount of time that this process has been scheduled in kernel mode,
    -- measured in clock ticks (divide by sysconf(_SC_CLK_TCK)).
  , starttime :: Int64
    -- ^ The time the process started after system boot. In kernels before Linux
    -- 2.6, this value was expressed in jiffies. Since Linux 2.6, the value is
    -- expressed in clock ticks (divide by sysconf(_SC_CLK_TCK)).
  , vsize :: Int64
    -- ^ Virtual memory size in bytes.
  , rss :: Int64
    -- ^ Resident Set Size: number of pages the process has in real memory. This
    -- is just the pages which count toward text, data, or stack space. This
    -- does not include pages which have not been demand-loaded in, or which are
    -- swapped out.
  }
  deriving
    ( Show )


{-|

A regular expression for parsing @/proc/xyz/stat@. See
@man 5 proc@ for more information on the format of this file:
https://linux.die.net/man/5/proc.

-}
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
