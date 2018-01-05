{-# language OverloadedStrings #-}

module Prometheus.Export.Text (
    exportMetricsAsText
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
    return $ Build.toLazyByteString $ foldMap exportSampleGroup samples

exportSampleGroup :: SampleGroup -> Build.Builder
exportSampleGroup (SampleGroup info ty samples) =
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
        escape '\\' = "\\\\"
        escape other = T.pack [other]

exportSamples :: [Sample] -> Build.Builder
exportSamples samples =
  mconcat [ exportSample s <> Build.charUtf8 '\n' | s <- samples ]

exportSample :: Sample -> Build.Builder
exportSample (Sample name labels value) =
  Build.byteString (T.encodeUtf8 name)
    <> (case labels of
         [] -> mempty
         l:ls ->
           Build.charUtf8 '{'
             <> exportLabel l
             <> mconcat [ Build.charUtf8 ',' <> exportLabel l' | l' <- ls ]
             <> Build.charUtf8 '}')
    <> Build.charUtf8 ' '
    <> Build.byteString value

exportLabel :: (Text, Text) -> Build.Builder
exportLabel (key, value) =
  Build.byteString (T.encodeUtf8 key)
    <> Build.charUtf8 '='
    <> Build.stringUtf8 (show value)
