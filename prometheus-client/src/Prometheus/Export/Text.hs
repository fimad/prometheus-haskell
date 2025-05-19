{-# language OverloadedStrings #-}

module Prometheus.Export.Text (
    exportMetricsAsText
,   exportMetricsAsOpenMetrics1
) where

import Prometheus.Info
import Prometheus.Metric
import Prometheus.Registry

import Control.Monad.IO.Class
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (foldMap)
import Data.Monoid ((<>), mempty, mconcat)
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
-- >>> exportMetricsAsText >>= Data.ByteString.Lazy.putStr
-- # HELP my_counter Example counter
-- # TYPE my_counter counter
-- my_counter 1.0
exportMetricsAsText :: MonadIO m => m BS.ByteString
exportMetricsAsText = do
    samples <- collectMetrics
    return $ Build.toLazyByteString $ foldMap (exportSampleGroup TextZeroZeroFour) samples

-- | Export all registered metrics in the OpenMetrics 1.0.0 format.
--
-- For the full specification of the format, see the official Prometheus
-- <https://prometheus.io/docs/specs/om/open_metrics_spec/ documentation>.
--
-- Note, you MUST set the content-type header to:
-- @application/openmetrics-text; version=1.0.0; charset=utf-8@
-- for this format.
--
-- The OpenMetrics spec lists more features than Prometheus actually supports.
-- The only additional benefit of OpenMetrics when using this library is that
-- exemplars are supported. 
exportMetricsAsOpenMetrics1 :: MonadIO m => m BS.ByteString
exportMetricsAsOpenMetrics1 = do
    samples <- collectMetrics
    return $ Build.toLazyByteString $ (foldMap (exportSampleGroup OpenMetricsOneZeroZero) samples) <> Build.byteString "# EOF\n"

data ExportFormat = TextZeroZeroFour | OpenMetricsOneZeroZero
  deriving (Show, Eq, Ord)

exportSampleGroup :: ExportFormat -> SampleGroup -> Build.Builder
exportSampleGroup format (SampleGroup info ty samples) =
    if null samples
        then mempty
        else prefix <> exportedSamples
    where
        exportedSamples = exportSamples samples
        name = metricName info
        help = metricHelp info
        prefix = Build.byteString $ T.encodeUtf8 $ T.unlines [
                "# HELP " <> name <> " " <> T.concatMap escape help
            ,   "# TYPE " <> name <> " " <> T.pack (show ty)
            ]
        escape '\n' = "\\n"
        escape '"' = if format == OpenMetricsOneZeroZero then "\\\"" else "\""
        escape '\\' = "\\\\"
        escape other = T.pack [other]

exportSamples :: [Sample] -> Build.Builder
exportSamples samples =
  mconcat [ exportSample s <> Build.charUtf8 '\n' | s <- samples ]

exportSample :: Sample -> Build.Builder
exportSample (Sample name labels value exemplarLabelPairs) =
  Build.byteString (T.encodeUtf8 name)
    <> buildLabelPairs labels
    <> Build.charUtf8 ' '
    <> Build.byteString value
    <> if null exemplarLabelPairs 
         then mempty 
         else Build.byteString " # " <> buildLabelPairs exemplarLabelPairs

  where buildLabelPairs labelPairs = case labelPairs of
         [] -> mempty
         l:ls ->
           Build.charUtf8 '{'
             <> exportLabel l
             <> mconcat [ Build.charUtf8 ',' <> exportLabel l' | l' <- ls ]
             <> Build.charUtf8 '}'

exportLabel :: (Text, Text) -> Build.Builder
exportLabel (key, value) =
  Build.byteString (T.encodeUtf8 key)
    <> Build.charUtf8 '='
    <> Build.stringUtf8 (show value)
