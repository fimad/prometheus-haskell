module Prometheus.Registry (
    register
,   unsafeRegister
,   dumpMetrics
) where

import Prometheus.Info
import Prometheus.Metric

import Control.Applicative ((<$>))
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS


data RegisteredMetric = forall s. MkRegisteredMetric (MetricDesc s, STM.TVar s)

type Registry = [RegisteredMetric]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar Registry
globalRegistry = unsafePerformIO $ STM.newTVarIO []

register :: MetricDesc s -> IO (Metric s)
register desc = STM.atomically $ do
    -- Force the metric descriptor to be evaluated. This allows errors returned
    -- by descriptor creating functions to be thrown on registration instead of
    -- at the first use of the metric.
    stateTVar <- desc `seq` STM.newTVar (descInitial desc)
    registry <- STM.readTVar globalRegistry
    let addToRegistry = (MkRegisteredMetric (desc, stateTVar) :)
    checkName registry desc $ STM.modifyTVar' globalRegistry addToRegistry
    return $ Metric $ STM.atomically . STM.modifyTVar' stateTVar

checkName :: Registry -> MetricDesc s -> a -> a
checkName registry newDesc r = foldl check r registry
    where
        check a (MkRegisteredMetric (desc, _))
            | (x:_) <- newName, not $ validStart x      = error errorInvalid
            | (_:xs) <- newName, not $ all validRest xs = error errorInvalid
            | ('_':'_':_) <- newName                    = error errorPrefix
            | [] <- newName                             = error errorEmpty
            | newName == nameOf desc                    = error errorDuplicate
            | otherwise                                 = a

        nameOf = metricName . descInfo
        newName = nameOf newDesc

        errorDuplicate = concat [
                "A metric with the name '"
            ,   newName
            ,   "' has already been registered."
            ]

        errorInvalid = concat [
                "The metric '", newName, "' contains invalid characters."
            ]

        errorPrefix = concat [
                "The metric '", newName, "' cannot start with '__'."
            ]

        errorEmpty = "Empty metric names are not allowed."

        validStart c =  ('a' <= c && c <= 'z')
                     || ('A' <= c && c <= 'Z')
                     || c == '_'
                     || c == ':'

        validRest c =  ('a' <= c && c <= 'z')
                    || ('A' <= c && c <= 'Z')
                    || ('0' <= c && c <= '9')
                    || c == '_'
                    || c == ':'

unsafeRegister :: MetricDesc s -> Metric s
unsafeRegister = unsafePerformIO . register

dumpMetrics :: IO LBS.ByteString
dumpMetrics = do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    dumps <- (++ [LBS.empty]) <$> mapM dumpMetric registry
    return $ LBS.concat $ intersperse (LBS.fromString "\n") $ dumps

dumpMetric :: RegisteredMetric -> IO LBS.ByteString
dumpMetric (MkRegisteredMetric (desc, stateTVar)) = do
    state <- STM.atomically $ STM.readTVar stateTVar
    let dump = descDump desc [] info state
    return $ if LBS.null dump then LBS.empty else prefix `LBS.append` dump
    where
        ty = descType desc
        info = descInfo desc
        name = metricName info
        help = metricHelp info
        prefix =  LBS.fromString $ unlines [
                "# HELP " ++ name ++ " " ++ help
            ,   "# TYPE " ++ name ++ " " ++ ty
            ]
