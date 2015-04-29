{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Prometheus (
    prometheus
) where

import Prometheus (exportMetricsAsText)
import qualified Data.ByteString.Builder as BS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


prometheus :: Wai.Middleware
prometheus app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == ["metrics"]
    then respondWithMetrics respond
    else app req respond

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
    metrics <- exportMetricsAsText
    respond $ Wai.responseBuilder HTTP.status200 headers $ BS.byteString metrics
    where
        headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
