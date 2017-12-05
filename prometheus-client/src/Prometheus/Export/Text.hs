{-# language OverloadedStrings #-}

module Prometheus.Export.Text (
    exportMetricsAsText
) where

import Prometheus.Info
import Prometheus.Label
import Prometheus.Metric
import Prometheus.Registry

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- $setup
-- >>> :module +Prometheus
-- >>> :set -XOverloadedStrings
-- >>> unregisterAll

-- | Export all registered metrics in the Prometheus 0.0.4 text exposition
-- format.
--
-- For the full specification of the format, see the official Prometheus
-- <http://prometheus.io/docs/instrumenting/exposition_formats/ documentation>.
--
-- >>> :m +Data.ByteString
-- >>> myCounter <- register $ counter (Info "my_counter" "Example counter")
-- >>> incCounter myCounter
-- >>> exportMetricsAsText >>= Data.ByteString.putStr
-- # HELP my_counter Example counter
-- # TYPE my_counter counter
-- my_counter 1.0
exportMetricsAsText :: MonadIO m => m BS.ByteString
exportMetricsAsText = do
    samples <- collectMetrics
    let exportedSamples = map exportSampleGroup samples ++ [BS.empty]
    return $ BS.concat $ intersperse (BS.fromString "\n") exportedSamples

exportSampleGroup :: SampleGroup -> BS.ByteString
exportSampleGroup (SampleGroup info ty samples) =
    if BS.null exportedSamples
        then BS.empty
        else prefix `BS.append` exportedSamples
    where
        exportedSamples = exportSamples samples
        name = metricName info
        help = metricHelp info
        prefix = T.encodeUtf8 $ T.unlines [
                "# HELP " <> name <> " " <> T.concatMap escape help
            ,   "# TYPE " <> name <> " " <> T.pack (show ty)
            ]
        escape '\n' = "\\n"
        escape '\\' = "\\\\"
        escape other = T.pack [other]

exportSamples :: [Sample] -> BS.ByteString
exportSamples = BS.intercalate (BS.fromString "\n") . map exportSample

exportSample :: Sample -> BS.ByteString
exportSample (Sample name [] value) = BS.concat [
        T.encodeUtf8 name, BS.fromString " ", value
    ]
exportSample (Sample name labels value) = BS.concat [
        T.encodeUtf8 name
    ,   BS.fromString "{", exportLabels labels, BS.fromString "} "
    ,   value
    ]

exportLabels :: LabelPairs -> BS.ByteString
exportLabels labels = T.encodeUtf8 $ T.intercalate "," $ map exportLabel labels

exportLabel :: (Text, Text) -> Text
exportLabel (key, value) = key <> "=" <> T.pack (show value)

