module Main where

import Test.DocTest

main :: IO ()
main = doctest [
        "-XExistentialQuantification"
    ,   "-XFlexibleInstances"
    ,   "-XGeneralizedNewtypeDeriving"
    ,   "-XTypeSynonymInstances"
    ,   "-isrc"
    ,   "Prometheus"
    ]
