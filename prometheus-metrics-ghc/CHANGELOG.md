# Change Log

## HEAD

## 1.0.1.2 -- 2021-08-26

- Increase allowed prometheus-client version ranges.

## 1.0.1.1

- Optimizations to measure GHC metrics with less polling.

## 1.0.1

- Allow GHC metrics to be labeled via `ghcMetricsWithLabels`.

## 1.0.0

- Supports prometheus-client-1.0.0

## 0.3.0

## 0.2.0

## 0.1.1

- Metrics that are always incrementing are now counters instead of gauges.
- Metric names have changed to be more inline with prometheus guidelines.
