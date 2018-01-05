-- This example demonstrates how to use the prometheus-haskell libraries to
-- instrument a simple web app that allows users to vote for their favorite
-- color.
{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P


{-# NOINLINE pageVisits #-}
pageVisits :: P.Counter
pageVisits = P.unsafeRegister
           $ P.counter
           -- Each metric provided by the base library takes an Info value that
           -- gives the name of the metric and a help string that describes the
           -- value that the metric represents.
           $ P.Info "page_visits" "The number of visits to the index page."

{-# NOINLINE votes #-}
votes :: P.Vector P.Label1 P.Counter
votes = P.unsafeRegister
      -- Declare a vector of counters with a single dimension: "vote".
      $ P.vector "vote"
      $ P.counter
      $ P.Info "votes" "The number of votes for each color."

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.
    _ <- P.register P.ghcMetrics
    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.
    run port (P.prometheus P.def app)

app :: Wai.Application
app request respond = do
    response <- case Wai.pathInfo request of
        []        -> doIndex
        ["red"]   -> doRed
        ["green"] -> doGreen
        ["blue"]  -> doBlue
        _         -> return $ mkResponse "404"
    respond response

mkResponse :: LBS.ByteString -> Wai.Response
mkResponse =  Wai.responseLBS status200 [(hContentType, "text/html")]

doIndex :: IO Wai.Response
doIndex = do
    P.incCounter pageVisits
    return $ mkResponse $ LBS.concat [
            "<a href='/metrics'>Metrics</a>"
        ,   "<br><br><br>"
        ,   "What's your favorite color?"
        ,   "<br>"
        ,   "<a href='/red'>Red!</a>"
        ,   "<br>"
        ,   "<a href='/blue'>Blue!</a>"
        ,   "<br>"
        ,   "<a href='/green'>Green!</a>"
        ]

doRed :: IO Wai.Response
doRed = do
    P.withLabel votes "red" P.incCounter
    return $ mkResponse "Red is alright I guess. <a href='/'>back</a>"

doBlue :: IO Wai.Response
doBlue = do
    P.withLabel votes "blue" P.incCounter
    return $ mkResponse "Blue is whatever. <a href='/'>back</a>"

doGreen :: IO Wai.Response
doGreen = do
    P.withLabel votes "green" P.incCounter
    return $ mkResponse "Green's ok. <a href='/'>back</a>"
