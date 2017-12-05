{-# language OverloadedStrings #-}

module Prometheus.Metric.VectorSpec (
    spec
) where

import Prometheus

import Control.Monad
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Vector" $ do
      it "starts with no labels" $ do
            m <- register $ vector ("a", "b") $ counter (Info "name" "help")
            value <- getVectorWith m getCounter
            value `shouldBe` []
      it "maintains state for a single label" $ do
            m <- register $ vector ("a", "b") $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel m ("foo", "bar") incCounter
            value <- getVectorWith m getCounter
            value `shouldBe` [(("foo", "bar"), 47)]
      it "maintains state for multiple labels" $ do
            m <- register $ vector "a" $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel m "foo" incCounter
            replicateM_ 42 $ withLabel m "bar" incCounter
            value <- getVectorWith m getCounter 
            value `shouldMatchList` [(T.pack "bar", 42), (T.pack "foo", 47)]
