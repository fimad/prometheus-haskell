# prometheus-haskell

This repository contains a collection of libraries that can be used to
instrument a Haskell application with metrics that can be consumed by the
[Prometheus](http://prometheus.io) monitoring system and time series database.

API documentation can be found on each libraries respective hackage page:
[prometheus-client](http://hackage.haskell.org/package/prometheus-client),
[prometheus-metrics-ghc](http://hackage.haskell.org/package/prometheus-metrics-ghc),
[wai-middleware-prometheus](http://hackage.haskell.org/package/wai-middleware-prometheus).

The purposes of the libraries are outline below, and an example application can
be found under the
[example](https://github.com/fimad/prometheus-haskell/tree/master/example)
folder in the root directory of the git repository.

## prometheus-client

This is the base library that defines the core data types and metrics. It has a
few dependencies and is intended to provide the minimum functionality required
to interact with Promethues.

## prometheus-metrics-ghc

This library provides custom metrics that an application can register that
expose information from GHC's runtime system. It is provided as a separate
package to keep the GHC specific dependencies out of the prometheus-client
library.

## wai-middleware-prometheus

This library provides WAI middleware that allows for easy integration of the
Prometheus client library into existing WAI apps. The middleware will by default
automatically instrument a WAI app with common HTTP metrics and respond to GET
requests on the /metrics endpoint.
