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
            m <- registerIO $ counter (Info "test_counter" "help string")
            incCounter m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_counter help string"
                ,   "# TYPE test_counter counter"
                ,   "test_counter 1"
                ])
      it "renders gauges" $ do
            m <- registerIO $ gauge (Info "test_gauge" "help string")
            setGauge 47 m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_gauge help string"
                ,   "# TYPE test_gauge gauge"
                ,   "test_gauge 47.0"
                ])
      it "renders summaries" $ do
            m <- registerIO $ summary (Info "metric" "help") defaultQuantiles
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
      it "renders histograms" $ do
            m <- registerIO $ histogram (Info "metric" "help") defaultBuckets
            observe 1.0 m
            observe 1.0 m
            observe 1.0 m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP metric help"
                ,   "# TYPE metric histogram"
                ,   "metric{le=\"0.005\"} 0"
                ,   "metric{le=\"0.01\"} 0"
                ,   "metric{le=\"0.025\"} 0"
                ,   "metric{le=\"0.05\"} 0"
                ,   "metric{le=\"0.1\"} 0"
                ,   "metric{le=\"0.25\"} 0"
                ,   "metric{le=\"0.5\"} 0"
                ,   "metric{le=\"1.0\"} 3"
                ,   "metric{le=\"2.5\"} 3"
                ,   "metric{le=\"5.0\"} 3"
                ,   "metric{le=\"10.0\"} 3"
                ,   "metric{le=\"+Inf\"} 3"
                ,   "metric_sum 3.0"
                ,   "metric_count 3"
                ])
      it "renders vectors" $ do
            m <- registerIO $ vector ("handler", "method")
                            $ counter (Info "test_counter" "help string")
            withLabel ("root", "GET") incCounter m
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP test_counter help string"
                ,   "# TYPE test_counter counter"
                ,   "test_counter{handler=\"root\",method=\"GET\"} 1"
                ])
      it "escapes newlines and slashes from help strings" $ do
            _ <- registerIO $ counter (Info "metric" "help \n \\string")
            result <- exportMetricsAsText
            result `shouldBe` BS.fromString (unlines [
                    "# HELP metric help \\n \\\\string"
                ,   "# TYPE metric counter"
                ,   "metric 0"
                ])
