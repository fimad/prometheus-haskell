module Prometheus.Metric.VectorSpec (
    spec
) where

import Prometheus

import Control.Monad
import Control.Applicative
import GHC.Exts (sortWith)
import Test.Hspec
import qualified Data.ByteString.UTF8 as BS

spec :: Spec
spec = describe "Prometheus.Metric.Vector" $ do
      it "starts with no labels" $ do
            m <- vector ("a", "b") $ counter (Info "name" "help")
            value <- getVectorWith getCounter m
            value `shouldBe` []
      it "maintains state for a single label" $ do
            m <- vector ("a", "b") $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel ("foo", "bar") incCounter m
            value <- getVectorWith getCounter m
            value `shouldBe` [(("foo", "bar"), 47)]
      it "maintains state for multiple labels" $ do
            m <- vector "a" $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel "foo" incCounter m
            replicateM_ 42 $ withLabel "bar" incCounter m
            value <- sortWith fst <$> getVectorWith getCounter m
            value `shouldBe` [("bar", 42), ("foo", 47)]
