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
-- | A counter models a monotonically increasing integer value. It is the
-- simplest type of metric provided by this library.
--
-- >>> myCounter <- counter (Info "my_counter" "An example counter")
-- >>> replicateM_ 47 (incCounter myCounter)
-- >>> getCounter myCounter
-- 47

,   Counter
,   counter
,   incCounter
,   getCounter

-- ** Gauge
--
-- | A gauge models an arbitrary floating point value. There are operations to
-- set the value of a gauge as well as add and subtract arbitrary values.
--
-- >>> myGauge <- gauge (Info "my_gauge" "An example gauge")
-- >>> setGauge 100 myGauge
-- >>> addGauge 50 myGauge
-- >>> subGauge 25 myGauge
-- >>> getGauge myGauge
-- 125.0

,   Gauge
,   gauge
,   incGauge
,   decGauge
,   addGauge
,   subGauge
,   setGauge
,   getGauge

-- ** Summary
--
-- | A summary captures observations of a floating point value over time and
-- summarizes the observations as a count, sum, and rank estimations. A typical
-- use case for summaries is measuring HTTP request latency.
--
-- >>> mySummary <- summary (Info "my_summary" "") defaultQuantiles
-- >>> observe 0 mySummary
-- >>> getSummary mySummary
-- [(1 % 2,0.0),(9 % 10,0.0),(99 % 100,0.0)]

,   Summary
,   Quantile
,   summary
,   defaultQuantiles
,   observe
,   getSummary

-- ** Vector
--
-- | A vector models a collection of metrics that share the same name but are
-- partitioned across a set of dimensions.
--
-- >>> myVector <- vector ("method", "code") $ counter (Info "http_requests" "")
-- >>> register myVector
-- >>> withLabel ("GET", "200") incCounter myVector
-- >>> withLabel ("GET", "200") incCounter myVector
-- >>> withLabel ("GET", "404") incCounter myVector
-- >>> withLabel ("POST", "200") incCounter myVector
-- >>> getVectorWith getCounter myVector
-- [(("GET","200"),2),(("GET","404"),1),(("POST","200"),1)]
-- >>> exportMetricsAsText >>= Data.ByteString.putStr
-- # HELP http_requests
-- # TYPE http_requests counter
-- http_requests{method="GET",code="200"} 2
-- http_requests{method="GET",code="404"} 1
-- http_requests{method="POST",code="200"} 1

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
-- >>> let myVector :: IO (Metric (Vector Label3 Counter));
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
-- >>> let cpuTimeMetric = Metric (MkCPUTime ()) collectCPUTime
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
-- >>> numAdds <- counter (Info "num_adds" "The number of additions")
-- >>> let add x y = incCounter numAdds >> return (x + y)
-- >>> let (3, updateMetrics) = runMonitor $ (add 1 1) >>= (add 1)
-- >>> getCounter numAdds
-- 0
-- >>> updateMetrics
-- >>> getCounter numAdds
-- 2

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
import Prometheus.Metric.Summary
import Prometheus.Metric.Vector
import Prometheus.MonadMonitor
import Prometheus.Registry


-- $setup
-- >>> :module +Prometheus
-- >>> :module +Control.Monad
-- >>> unregisterAll
