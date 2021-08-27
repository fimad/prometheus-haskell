# Change Log

## HEAD

## 1.1.0 -- 2021-08-26

- Replaced Summary implementation with `data-sketches` which should result in
  significant performance improvements.

## 1.0.1 -- 2020-07-26

Expose the `Bucket` type

## 1.0.0.1 -- 2020-05-27

- Optimizations to improve memory usage.

## 1.0.0 -- 2018-08-10

Version 1.0.0 is a significant rewrite of some core parts of the API, as
`prometheus-effect` has been deprecated in favour of this library.

### Breaking Changes

#### New Metric API

The most substatial change regards the `Metric` interface. Unregistered counters
are now wrapped in the `Metric` type (as before), but when registered, the
`Metric` wrapper is removed. This change means that registered metrics are more
lightweight (they are often just a newtype wrapper), but also helps users
distinguish between registered and unregistered metrics.

To adapt to this change, users should:

* Store and pass registered metrics. Rather than passing `Metric Counter`,
  prefer `register`ing to obtain a `Counter` and pass that around.

* Top level registrations will no longer return `Metric`. If you have

    ```haskell
    httpRequests :: Metric Counter
    httpRequests = unsafeRegisterIO $ counter (Info ...)
    ```

    This should change to

    ```haskell
    httpRequests :: Counter
    httpRequests = unsafeRegister $ counter (Info ...)
    ```

#### Other Breaking Changes

* `Prometheus.exportMetricsAsText` now returns a lazy `ByteString` rather than a
  strict `ByteString`. This provides better streaming performance, and the
  underlying `ByteString` is built using a `Builder` for improved performance.
  You can convert the lazy `ByteString` into a strict one using
  `Data.ByteString.Lazy.toStrict`, but you should consider taking advantage of
  the lazy interface if possible.

* `Prometheus.Info` now uses `Text` instead of `String`. It is recommended
  that you use the `OverloadedStrings` extension.

* The label interface has been changed to only support tuples of `Text`.
  The tuple instances prevent any other tuple instances from being defined. This
  interface is in flux and may change again in the future. For now,
  `OverloadedStrings` will give you compatibility with the old interface.


### Backwards Compatible Changes

* Many functions that were specialised to `IO` have been generalised to work in
  `MonadIO`.

### Additions

* `Prometheus.countExceptions` to count the amount of exceptions a given monadic
  action throws.


## 0.3.0

- Change exported name of histogram metrics to have `_bucket` suffix.

## 0.2.0

- Add the histogram metric type (#15).
- A monitoric clock is used for duratoin measurments instead of UTCTime (#9,
  #16).

## 0.1.1

- Counters are now represented as floating point numbers instead of integers.
- New counter methods `addDurationToCounter` and `addCounter`.
