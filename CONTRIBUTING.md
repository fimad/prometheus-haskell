# How To Contribute

This document describes how to contribute to prometheus-haskell including
development environment setup and requirements for pull requests.

## Included Libraries

The prometheus-haskell repository contains several libraries.

  - prometheus-client - This is the base library that defines the core data
    types and metrics. It has a few dependencies and is intended to provide the
    minimum functionality requried to interact with Promethues.

  - prometheus-metrics-ghc - This library provides several custom metrics that
    expose information from GHC's runtime system.

  - wai-middleware-prometheus - This library provides WAI middleware that allows
    for easy integration of the prometheus client library into existing WAI
    apps.

## Development Environment Setup

The recommended environment consists of creating a shared cabal sandbox in the
root of this repository and configuring each library to use this shared sandbox.
Running the following from the root of this repository will setup an appropriate
sandbox environment.

    cabal sandbox init
    cabal sandbox add-source prometheus-client
    cabal sandbox add-source prometheus-metrics-ghc
    cabal sandbox add-source wai-middleware-prometheus

    cd prometheus-client
    cabal sandbox init --sandbox ../.cabal-sandbox

    cd ../prometheus-metrics-ghc
    cabal sandbox init --sandbox ../.cabal-sandbox

    cd ../wai-middleware-prometheus
    cabal sandbox init --sandbox ../.cabal-sandbox

### Install dependencies

The following needs to be run at least once for each library you intend to work
on. It will need to be run again if you add new dependencies.

    cabal configure --enable-benchmarks --enable-tests --enable-library-coverage
    cabal install --enable-tests --enable-benchmarks --dependencies-only

### Build

    cabal build --ghc-options="-Werror"

### Running Tests

    cabal test --ghc-options="-Werror"

### Running Benchmarks

    cabal bench --ghc-options="-Werror" --benchmark-options="-o report.html

## Pull Requests

All pull requests should pass hlint with no errors and build cleanly when
supplying -Werror to GHC. Additionally all substantial new features should be
accompanied by unit tests.
