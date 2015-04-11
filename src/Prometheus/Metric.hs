module Prometheus.Metric (
    Metric (..)
,   MetricDesc (..)
,   makeMetric
,   defaultMetricDump

,   MonadMetric (..)
,   MetricT
,   runMetricT
) where

import Prometheus.Info
import Prometheus.Label

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.List (intercalate)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS


data MetricDesc s = MetricDesc {
        descDump    :: LabelPairs -> Info -> s -> LBS.ByteString
    ,   descInfo    :: Info
    ,   descInitial :: s
    ,   descType    :: String
    }

data Metric s = Metric {
        metricDump   :: LabelPairs -> Info -> s -> LBS.ByteString
    ,   metricInfo   :: Info
    ,   metricState  :: STM.TVar s
    ,   metricType   :: String
    ,   metricModify :: MonadMetric m => Metric s -> (s -> s) -> m ()
    }

class Monad m => MonadMetric m where
    withMetric :: Metric s -> (s -> s) -> m ()

instance MonadMetric IO where
    withMetric metric f = STM.atomically $ do
        let stateTVar = metricState metric
        STM.modifyTVar' stateTVar f

data MetricOperation = forall s. MkMetricOperation (Metric s) (s -> s)

newtype MetricT m a= MkMetricT (WriterT [MetricOperation] m a)
    deriving (Applicative, Functor, Monad)

instance Monad m => MonadMetric (MetricT m) where
    withMetric metric f = MkMetricT $ tell [MkMetricOperation metric f]

runMetricT :: MonadIO io => MetricT io a -> io a
runMetricT (MkMetricT writerT) = do
    (v, operations) <- runWriterT writerT
    mapM_ applyOperation operations
    return v

applyOperation :: MonadIO io => MetricOperation -> io ()
applyOperation (MkMetricOperation metric f) = liftIO $ STM.atomically $ do
    let stateTVar = metricState metric
    STM.modifyTVar' stateTVar f

makeMetric :: MetricDesc s -> IO (Metric s)
makeMetric desc = STM.atomically $ do
    stateTVar <- STM.newTVar (descInitial desc)
    return Metric {
            metricDump   = descDump desc
        ,   metricInfo   = descInfo desc
        ,   metricState  = stateTVar
        ,   metricType   = descType desc
        ,   metricModify = withMetric
        }

defaultMetricDump :: Show s => LabelPairs -> Info -> s -> LBS.ByteString
defaultMetricDump [] info value = LBS.fromString $ concat [
        metricName info, " ", show value
    ]
defaultMetricDump labels info value = LBS.fromString $ concat [
        metricName info, "{", dumpLabels labels, "} ", show value
    ]

dumpLabels :: LabelPairs -> String
dumpLabels labels = intercalate "," $ map dumpLabel labels

dumpLabel :: (String, String) -> String
dumpLabel (key, value) = key ++ "=" ++ show value
