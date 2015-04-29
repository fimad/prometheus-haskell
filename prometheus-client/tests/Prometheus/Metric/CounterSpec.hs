module Prometheus.Metric.CounterSpec (
    spec
) where

import Prometheus

import Control.Monad
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Counter" $ do
      it "starts at zero" $ do
            m <- counter (Info "name" "help")
            value <- getCounter m
            value `shouldBe` 0
      it "increments correctly" $ do
            m <- counter (Info "name" "help")
            replicateM_ 47 (incCounter m)
            value <- getCounter m
            value `shouldBe` 47
