module Main where

import Test.DocTest

main :: IO ()
main = doctest [
        "-isrc"
    ,   "Network.Wai.Middleware.Prometheus"
    ]
