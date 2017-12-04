-- | This module provides the basics for instrumenting Haskell executables for
-- use with the <http://prometheus.io/ Prometheus> monitoring system.
module Prometheus (

-- * Registry

    registerIO
,   register
,   unsafeRegisterIO
,   unsafeRegister
,   unregisterAll
,   collectMetrics

-- * Exporting

,   exportMetricsAsText

-- * Metrics
--
-- | A metric represents a single value that is being monitored. For example
-- a metric could be the number of open files, the current CPU temperature, the
-- elapsed time of execution, and the latency of HTTP requests.
--
-- This module provides 4 built-in metric types: counters, gauges, summaries,
-- and metric vectors. These types of metrics should cover most typical use
-- cases. However, for more specialized use cases it  is also possible to write
-- custom metrics.

-- ** Counter
--
-- | A counter models a monotonically increasing value. It is the simplest
-- type of metric provided by this library.
--
-- A Counter is typically used to count requests served, tasks completed,
-- errors occurred, etc.
--
-- >>> myCounter <- register $ counter (Info "my_counter" "An example counter")
-- >>> replicateM_ 47 (incCounter myCounter)
-- >>> getCounter myCounter
-- 47.0
-- >>> void $ addCounter myCounter 10
-- >>> getCounter myCounter
-- 57.0

,   Counter
,   counter
,   incCounter
,   addCounter
,   unsafeAddCounter
,   addDurationToCounter
,   getCounter

-- ** Gauge
--
-- | A gauge models an arbitrary floating point value. There are operations to
-- set the value of a gauge as well as add and subtract arbitrary values.
--
-- >>> myGauge <- register $ gauge (Info "my_gauge" "An example gauge")
-- >>> setGauge myGauge 100 
-- >>> addGauge myGauge 50
-- >>> subGauge myGauge 25
-- >>> getGauge myGauge
-- 125.0

,   Gauge
,   gauge
,   incGauge
,   decGauge
,   addGauge
,   subGauge
,   setGauge
,   setGaugeToDuration
,   getGauge

-- ** Summaries and histograms
--
-- | An 'Observer' is a generic metric that captures observations of a
-- floating point value over time. Different implementations can store
-- and summarise these value in different ways.
--
-- The two main observers are summaries and histograms. A 'Summary' allows you
-- to get a precise estimate of a particular quantile, but cannot be meaningfully
-- aggregated across processes. A 'Histogram' packs requests into user-supplied
-- buckets, which /can/ be aggregated meaningfully, but provide much less precise
-- information on particular quantiles.

,   Observer(..)
,   observeDuration

-- *** Summary
--
-- | A summary is an 'Observer' that summarizes the observations as a count,
-- sum, and rank estimations. A typical use case for summaries is measuring
-- HTTP request latency.
--
-- >>> mySummary <- register $ summary (Info "my_summary" "") defaultQuantiles
-- >>> observe mySummary 0
-- >>> getSummary mySummary
-- [(1 % 2,0.0),(9 % 10,0.0),(99 % 100,0.0)]

,   Summary
,   Quantile
,   summary
,   defaultQuantiles
,   getSummary

-- *** Histogram
--
-- | A histogram captures observations of a floating point value over time
-- and stores those observations in a user-supplied histogram. A typical use case
-- for histograms is measuring HTTP request latency. Histograms are unlike
-- summaries in that they can be meaningfully aggregated across processes.
--
-- >>> myHistogram <- register $ histogram (Info "my_histogram" "") defaultBuckets
-- >>> observe myHistogram 0 
-- >>> getHistogram myHistogram
-- fromList [(5.0e-3,1),(1.0e-2,0),(2.5e-2,0),(5.0e-2,0),(0.1,0),(0.25,0),(0.5,0),(1.0,0),(2.5,0),(5.0,0),(10.0,0)]
,   Histogram
,   histogram
,   defaultBuckets
,   exponentialBuckets
,   linearBuckets
,   getHistogram

-- ** Vector
--
-- | A vector models a collection of metrics that share the same name but are
-- partitioned across a set of dimensions.
--
-- >>> myVector <- register $ vector ("method", "code") $ counter (Info "http_requests" "")
-- >>> withLabel myVector ("GET", "200") incCounter 
-- >>> withLabel myVector ("GET", "200") incCounter 
-- >>> withLabel myVector ("GET", "404") incCounter 
-- >>> withLabel myVector ("POST", "200") incCounter 
-- >>> getVectorWith myVector getCounter 
-- [(("GET","200"),2.0),(("GET","404"),1.0),(("POST","200"),1.0)]
-- >>> exportMetricsAsText >>= Data.ByteString.putStr
-- # HELP http_requests
-- # TYPE http_requests counter
-- http_requests{method="GET",code="200"} 2.0
-- http_requests{method="GET",code="404"} 1.0
-- http_requests{method="POST",code="200"} 1.0

,   Vector
,   vector
,   withLabel
,   removeLabel
,   clearLabels
,   getVectorWith

-- *** Labels
--
-- | The labels of a vector metric are types of the class Label. This module
-- defines all n-tupes of Strings for n <= 9 to be Labels. Additionally, the
-- type aliases LabelN is defined for each of these tuple types to make
-- specifying the types of vectors more concise.
--
-- >>> :{
-- >>> let myVector :: Metric (Vector Label3 Counter);
-- >>>     myVector = vector ("a", "b", "c") $ counter (Info "some_counter" "")
-- >>> :}

,   Label (..)
,   LabelPairs
,   Label0
,   Label1
,   Label2
,   Label3
,   Label4
,   Label5
,   Label6
,   Label7
,   Label8
,   Label9

-- ** Custom metrics
--
-- | Custom metrics can be created by directly creating a new 'Metric' type. There
-- are two parts of any metric, the handle and the collect method.
--
-- The handle is a value embedded in the metric that is intended to allow for
-- communication with the metric from instrumented code. For example, all of the
-- metrics provided by this library use a newtype wrapped TVar of some
-- underlying data type as their handle. When defining a new metric, it is
-- recommended that you use a newtype wrapper around your handle type as it will
-- allow users of your metric to succinctly identify your metric in type
-- signatures.
--
-- The collect method is responsible for serializing the current value of
-- a metric into a list of 'SampleGroup's.
--
-- The following is an example of a custom metric that models the current CPU
-- time. It uses a newtype wrapped unit as the handler type since it doesn't
-- need to maintain any state.
--
-- >>> :m +System.CPUTime
-- >>> :m +Data.ByteString.UTF8
-- >>> newtype CPUTime = MkCPUTime ()
-- >>> let info = Info "cpu_time" "The current CPU time"
-- >>> let toValue = Data.ByteString.UTF8.fromString . show
-- >>> let toSample = Sample "cpu_time" [] . toValue
-- >>> let toSampleGroup = (:[]) . SampleGroup info GaugeType . (:[]) . toSample
-- >>> let collectCPUTime = fmap toSampleGroup getCPUTime
-- >>> let cpuTimeMetric = Metric (return (MkCPUTime (), collectCPUTime))
-- >>> register cpuTimeMetric
-- >>> exportMetricsAsText >>= Data.ByteString.putStr
-- # HELP cpu_time The current CPU time
-- # TYPE cpu_time gauge
-- cpu_time ...

-- * Instrumenting pure code
--
-- | Pure code can be instrumented through the use of the 'Monitor' monad and
-- 'MonitorT' monad transformer. These constructs work by queueing all
-- operations on metrics. In order for the operations to actually be performed,
-- the queue must be evaluated within the IO monad.
--
-- The following is a contrived example that defines an add function that
-- records the number of times it was invoked.
--
-- > add :: Int -> Int -> Monitor Int
--
-- Note that the changes to numAdds are not reflected until the updateMetrics
-- value has been evaluated in the IO monad.
--
-- >>> numAdds <- register $ counter (Info "num_adds" "The number of additions")
-- >>> let add x y = incCounter numAdds >> return (x + y)
-- >>> let (3, updateMetrics) = runMonitor $ (add 1 1) >>= (add 1)
-- >>> getCounter numAdds
-- 0.0
-- >>> updateMetrics
-- >>> getCounter numAdds
-- 2.0

,   MonadMonitor (..)
,   Monitor
,   runMonitor
,   MonitorT
,   runMonitorT

-- * Base data types

,   Info (..)
,   Metric (..)
,   Sample (..)
,   SampleGroup (..)
,   SampleType (..)
) where

import Prometheus.Export.Text
import Prometheus.Info
import Prometheus.Label
import Prometheus.Metric
import Prometheus.Metric.Counter
import Prometheus.Metric.Gauge
import Prometheus.Metric.Histogram
import Prometheus.Metric.Observer
import Prometheus.Metric.Summary
import Prometheus.Metric.Vector
import Prometheus.MonadMonitor
import Prometheus.Registry


-- $setup
-- >>> :module +Prometheus
-- >>> :module +Control.Monad
-- >>> unregisterAll
