{-# language OverloadedStrings #-}

module Prometheus.Metric.GaugeSpec (
    spec
) where

import Prometheus

import Control.Monad
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Gauge" $ do
      it "starts at zero" $ do
            m <- register $ gauge (Info "name" "help")
            value <- getGauge m
            value `shouldBe` 0
      it "adds correctly" $ do
            m <- register $ gauge (Info "name" "help")
            replicateM_ 47 (addGauge m 1)
            value <- getGauge m
            value `shouldBe` 47
      it "subtracts correctly" $ do
            m <- register $ gauge (Info "name" "help")
            replicateM_ 47 (addGauge m (-1.0))
            value <- getGauge m
            value `shouldBe` (-47.0)
      it "can be reset" $ do
            m <- register $ gauge (Info "name" "help")
            setGauge m 1 
            setGauge m 2
            value <- getGauge m
            value `shouldBe` 2
