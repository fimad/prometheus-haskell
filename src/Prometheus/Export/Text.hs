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
    let exportedSamples = map exportSample samples ++ [BS.empty]
    return $ BS.concat $ intersperse (BS.fromString "\n") exportedSamples

exportSample :: Sample -> BS.ByteString
exportSample (Sample info ty subsamples) =
    if BS.null exportedSubsambples
        then BS.empty
        else prefix `BS.append` exportedSubsambples
    where
        exportedSubsambples = exportSubSamples info subsamples
        name = metricName info
        help = metricHelp info
        prefix =  BS.fromString $ unlines [
                "# HELP " ++ name ++ " " ++ help
            ,   "# TYPE " ++ name ++ " " ++ show ty
            ]

exportSubSamples :: Info -> [(LabelPairs, BS.ByteString)] -> BS.ByteString
exportSubSamples info = BS.intercalate (BS.fromString "\n") . map (exportSubSample info)

exportSubSample :: Info -> (LabelPairs, BS.ByteString) -> BS.ByteString
exportSubSample info ([], value) = BS.concat [
        BS.fromString $ metricName info, BS.fromString " ", value
    ]
exportSubSample info (labels, value) = BS.concat [
        BS.fromString $ metricName info
    ,   BS.fromString "{", exportLabels labels, BS.fromString "} "
    ,   value
    ]

exportLabels :: LabelPairs -> BS.ByteString
exportLabels labels = BS.fromString $ intercalate "," $ map exportLabel labels

exportLabel :: (String, String) -> String
exportLabel (key, value) = key ++ "=" ++ show value

