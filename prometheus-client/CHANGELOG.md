# Change Log

## HEAD

## 0.3.0

- Change exported name of histogram metrics to have `_bucket` suffix.

## 0.2.0

- Add the histogram metric type (#15).
- A monitoric clock is used for duratoin measurments instead of UTCTime (#9,
  #16).

## 0.1.1

- Counters are now represented as floating point numbers instead of integers.
- New counter methods `addDurationToCounter` and `addCounter`.
