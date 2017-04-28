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


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

-- | Export all registered metrics in the Prometheus 0.0.4 text exposition
-- format.
--
-- For the full specification of the format, see the official Prometheus
-- <http://prometheus.io/docs/instrumenting/exposition_formats/ documentation>.
--
-- >>> :m +Data.ByteString
-- >>> myCounter <- registerIO $ counter (Info "my_counter" "Example counter")
-- >>> incCounter myCounter
-- >>> exportMetricsAsText >>= Data.ByteString.putStr
-- # HELP my_counter Example counter
-- # TYPE my_counter counter
-- my_counter 1.0
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
                "# HELP " ++ name ++ " " ++ escape help
            ,   "# TYPE " ++ name ++ " " ++ show ty
            ]
        escape []        = []
        escape ('\n':xs) = '\\' : 'n' : escape xs
        escape ('\\':xs) = '\\' : '\\' : escape xs
        escape (x:xs)    = x : escape xs

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

