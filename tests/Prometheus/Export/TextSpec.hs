module Prometheus.Export.TextSpec (
    spec
) where

import Prometheus

import Test.Hspec
import qualified Data.ByteString.UTF8 as BS

spec :: Spec
spec = before_ unregisterAll $ after_ unregisterAll $
  describe "Prometheus.Export.Text.exportMetricsAsText" $ do
      it "renders counters" $ do
            m <- register $ counter (Info "test_counter" "help string")
            incCounter m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_counter help string"
                ,   "# TYPE test_counter counter"
                ,   "test_counter 1"
                ])
      it "renders gauges" $ do
            m <- register $ gauge (Info "test_gauge" "help string")
            setGauge 47 m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_gauge help string"
                ,   "# TYPE test_gauge gauge"
                ,   "test_gauge 47.0"
                ])
      it "renders summaries" $ do
            m <- register $ summary (Info "metric" "help") defaultQuantiles
            observe 1 m
            observe 1 m
            observe 1 m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP metric help"
                ,   "# TYPE metric summary"
                ,   "metric{quantile=\"0.5\"} 1.0"
                ,   "metric{quantile=\"0.9\"} 1.0"
                ,   "metric{quantile=\"0.99\"} 1.0"
                ,   "metric_sum 3.0"
                ,   "metric_count 3"
                ])
      it "renders vectors" $ do
            m <- register $ vector ("handler", "method")
                          $ counter (Info "test_counter" "help string")
            withLabel ("root", "GET") incCounter m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_counter help string"
                ,   "# TYPE test_counter counter"
                ,   "test_counter{handler=\"root\",method=\"GET\"} 1"
                ])
      it "escapes newlines and slashes from help strings" $ do
            m <- register $ counter (Info "metric" "help \n \\string")
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP metric help \\n \\\\string"
                ,   "# TYPE metric counter"
                ,   "metric 0"
                ])
