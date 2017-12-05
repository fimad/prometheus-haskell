module Prometheus.Info (
    Info (..)
,   checkInfo
) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Meta data about a metric including its name and a help string that
-- describes the value that the metric is measuring.
data Info = Info {
    metricName :: Text
,   metricHelp :: Text
} deriving (Read, Show, Eq, Ord)

checkInfo :: Info -> a -> a
checkInfo info a
    | (x:_)       <- T.unpack name, not $ validStart x     = errorInvalid
    | (_:xs)      <- T.unpack name, not $ all validRest xs = errorInvalid
    | ('_':'_':_) <- T.unpack name                         = errorPrefix
    | []          <- T.unpack name                         = errorEmpty
    | otherwise                                   = a
    where
        name = metricName info

        errorInvalid = error $ concat [
                "The metric '", T.unpack name, "' contains invalid characters."
            ]

        errorPrefix = error $ concat [
                "The metric '", T.unpack name, "' cannot start with '__'."
            ]

        errorEmpty = error "Empty metric names are not allowed."

        validStart c =  ('a' <= c && c <= 'z')
                     || ('A' <= c && c <= 'Z')
                     || c == '_'
                     || c == ':'

        validRest c =  ('a' <= c && c <= 'z')
                    || ('A' <= c && c <= 'Z')
                    || ('0' <= c && c <= '9')
                    || c == '_'
                    || c == ':'
