{-# language OverloadedStrings #-}

module Prometheus.Metric.CounterSpec (
    spec
) where

import Prometheus

import Control.Exception
import Control.Monad
import Test.Hspec

spec :: Spec
spec = describe "Prometheus.Metric.Counter" $ do
      it "starts at zero" $ do
            m <- register $ counter (Info "name" "help")
            value <- getCounter m
            value `shouldBe` 0
      it "increments correctly" $ do
            m <- register $ counter (Info "name" "help")
            replicateM_ 47 (incCounter m)
            value <- getCounter m
            value `shouldBe` 47
      it "counts exceptions" $ do
            m <- register $ counter (Info "name" "help")
            replicateM_ 10 $ do
              Left SomeException{} <- try $ countExceptions m $ error "Boom!"
              return ()
            getCounter m `shouldReturn` 10
