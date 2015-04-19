module Prometheus.Metric.GaugeSpec (
    spec
) where

import Prometheus

import Control.Monad
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Gauge" $ do
      it "starts at zero" $ do
            m <- gauge (Info "name" "help")
            value <- getGauge m
            value `shouldBe` 0
      it "adds correctly" $ do
            m <- gauge (Info "name" "help")
            replicateM_ 47 (addGauge 1.0 m)
            value <- getGauge m
            value `shouldBe` 47
      it "subtracts correctly" $ do
            m <- gauge (Info "name" "help")
            replicateM_ 47 (addGauge (-1.0) m)
            value <- getGauge m
            value `shouldBe` (-47.0)
      it "can be reset" $ do
            m <- gauge (Info "name" "help")
            setGauge 1 m
            setGauge 2 m
            value <- getGauge m
            value `shouldBe` 2
