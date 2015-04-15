module Prometheus.Export.Text (
    exportMetricsAsText
) where

import Prometheus.Info
import Prometheus.Label
import Prometheus.Metric
import Prometheus.Registry

import Data.List (intersperse, intercalate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


exportMetricsAsText :: IO BS.ByteString
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
        prefix =  BS.fromString $ unlines [
                "# HELP " ++ name ++ " " ++ help
            ,   "# TYPE " ++ name ++ " " ++ show ty
            ]

exportSamples :: [Sample] -> BS.ByteString
exportSamples = BS.intercalate (BS.fromString "\n") . map exportSample

exportSample :: Sample -> BS.ByteString
exportSample (Sample name [] value) = BS.concat [
        BS.fromString name, BS.fromString " ", value
    ]
exportSample (Sample name labels value) = BS.concat [
        BS.fromString name
    ,   BS.fromString "{", exportLabels labels, BS.fromString "} "
    ,   value
    ]

exportLabels :: LabelPairs -> BS.ByteString
exportLabels labels = BS.fromString $ intercalate "," $ map exportLabel labels

exportLabel :: (String, String) -> String
exportLabel (key, value) = key ++ "=" ++ show value

