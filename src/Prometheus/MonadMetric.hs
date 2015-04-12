module Prometheus.MonadMetric (
    MonadMetric (..)
,   MetricT
,   runMetricT
) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)


class Monad m => MonadMetric m where
    doIO :: IO () -> m ()

instance MonadMetric IO where
    doIO = id

newtype MetricT m a= MkMetricT (WriterT [IO ()] m a)
    deriving (Applicative, Functor, Monad, MonadTrans)

instance Monad m => MonadMetric (MetricT m) where
    doIO f = MkMetricT $ tell [f]

runMetricT :: MonadIO io => MetricT io a -> io a
runMetricT (MkMetricT writerT) = do
    (v, operations) <- runWriterT writerT
    liftIO $ sequence_ operations
    return v
