module Prometheus.Info (
    Info (..)
,   checkInfo
) where


-- | Meta data about a metric including its name and a help string that
-- describes the value that the metric is measuring.
data Info = Info {
    metricName :: String
,   metricHelp :: String
} deriving (Read, Show, Eq, Ord)

checkInfo :: Info -> a -> a
checkInfo info a
    | (x:_)       <- name, not $ validStart x     = errorInvalid
    | (_:xs)      <- name, not $ all validRest xs = errorInvalid
    | ('_':'_':_) <- name                         = errorPrefix
    | []          <- name                         = errorEmpty
    | otherwise                                   = a
    where
        name = metricName info

        errorInvalid = error $ concat [
                "The metric '", name, "' contains invalid characters."
            ]

        errorPrefix = error $ concat [
                "The metric '", name, "' cannot start with '__'."
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
