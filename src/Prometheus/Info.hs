module Prometheus.Info (
    Info (..)
) where


data Info = Info {
    metricName :: String
,   metricHelp :: String
} deriving (Read, Show, Eq, Ord)
