{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prometheus.MonadMonitor (
    MonadMonitor (..)
,   Monitor
,   runMonitor
,   MonitorT
,   runMonitorT
) where

import Control.Applicative (Applicative)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Monoid (Monoid)


-- | MonadMonitor describes a class of Monads that are capable of performing
-- asynchronous IO operations.
class Monad m => MonadMonitor m where
    doIO :: IO () -> m ()
    default doIO :: (MonadTrans t, MonadMonitor n, m ~ t n) => IO () -> m ()
    doIO = lift . doIO

instance MonadMonitor IO where
    doIO = id

instance (MonadMonitor m) => MonadMonitor (ExceptT e m)
instance (MonadMonitor m) => MonadMonitor (IdentityT m)
instance (MonadMonitor m) => MonadMonitor (MaybeT m)
instance (MonadMonitor m, Monoid w) => MonadMonitor (L.RWST r w s m)
instance (MonadMonitor m, Monoid w) => MonadMonitor (S.RWST r w s m) 
instance (MonadMonitor m) => MonadMonitor (ReaderT r m)
instance (MonadMonitor m) => MonadMonitor (L.StateT s m)
instance (MonadMonitor m) => MonadMonitor (S.StateT s m) 
instance (MonadMonitor m, Monoid w) => MonadMonitor (L.WriterT w m)
instance (MonadMonitor m, Monoid w) => MonadMonitor (S.WriterT w m) 

-- | Monitor allows the use of Prometheus metrics in pure code. When using
-- Monitor, all of the metric operations will be collected and queued into
-- a single IO () value that can be run from impure code.
--
-- Because all of the operations are performed asynchronously use of this class
-- is not recommended for use with metrics that are time sensitive (e.g. for
-- measuring latency).
type Monitor a = MonitorT Identity a

-- | MonitorT is the monad transformer analog of Monitor and allows for
-- monitoring pure monad transformer stacks.
newtype MonitorT m a = MkMonitorT (WriterT [IO ()] m a)
    deriving (Applicative, Functor, Monad, MonadTrans)

instance Monad m => MonadMonitor (MonitorT m) where
    doIO f = MkMonitorT $ tell [f]

-- | Extract a value and the corresponding monitor update value from the Monitor
-- monad. For an example use see 'Monitor'.
runMonitor :: Monitor a -> (a, IO ())
runMonitor a = runIdentity $ runMonitorT a

-- | Extract a value and the corresponding monitor update value from the
-- MonitorT monad transformer.
runMonitorT :: Monad m => MonitorT m a -> m (a, IO ())
runMonitorT (MkMonitorT writerT) = do
    (v, operations) <- runWriterT writerT
    return (v, sequence_ operations)
