{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Prometheus.Label (
    Label (..)
,   LabelPairs(..)
,   LabelPair(..)
,   Label0
,   Label1
,   Label2
,   Label3
,   Label4
,   Label5
,   Label6
,   Label7
,   Label8
,   Label9
) where

import Data.Text

-- | A list of tuples where the first value is the label and the second is the
-- value of that label.
newtype LabelPairs = LabelPairs { unLabelPairs :: [LabelPair] }
    deriving stock Show
    deriving newtype (Semigroup, Monoid)

data LabelPair = LabelPair { labelKey :: !Text, labelValue :: !Text }
    deriving stock Show

-- | Label describes a class of types that can be used to as the label of
-- a vector.
class Ord l => Label l where
    labelPairs :: l -> l -> LabelPairs

type Label0 = ()

instance Label () where
    labelPairs () () = LabelPairs mempty

type Label1 = Text

instance Label Text where
    labelPairs !key !value = LabelPairs [LabelPair key value]

type Label2 = (Text, Text)

instance (a ~ Text, b ~ a) => Label (a, b)  where
    labelPairs (!k1, !k2) (!v1, !v2) = LabelPairs [LabelPair k1 v1, LabelPair k2 v2]

type Label3 = (Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a) => Label (a, b, c)  where
    labelPairs (!k1, !k2, !k3) (!v1, !v2, !v3) = LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3]

type Label4 = (Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a) => Label (a, b, c, d)  where
    labelPairs (!k1, !k2, !k3, !k4) (!v1, !v2, !v3, !v4) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4]

type Label5 = (Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a) => Label (a, b, c, d, e)  where
    labelPairs (!k1, !k2, !k3, !k4, !k5) (!v1, !v2, !v3, !v4, !v5) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4, LabelPair k5 v5]

type Label6 = (Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a) => Label (a, b, c, d, e, f)  where
    labelPairs (!k1, !k2, !k3, !k4, !k5, !k6) (!v1, !v2, !v3, !v4, !v5, !v6) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4, LabelPair k5 v5, LabelPair k6 v6]

type Label7 = (Text, Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a) => Label (a, b, c, d, e, f, g)  where
    labelPairs (!k1, !k2, !k3, !k4, !k5, !k6, !k7) (!v1, !v2, !v3, !v4, !v5, !v6, !v7) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4, LabelPair k5 v5, LabelPair k6 v6,
             LabelPair k7 v7]

type Label8 = (Text, Text, Text, Text, Text, Text, Text, Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a) => Label (a, b, c, d, e, f, g, h) where
    labelPairs (!k1, !k2, !k3, !k4, !k5, !k6, !k7, !k8)
               (!v1, !v2, !v3, !v4, !v5, !v6, !v7, !v8) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4, LabelPair k5 v5, LabelPair k6 v6,
             LabelPair k7 v7, LabelPair k8 v8]

type Label9 = (Text, Text, Text, Text, Text, Text, Text, Text,
               Text)

instance (a ~ Text, b ~ a, c ~ a, d ~ a, e ~ a, f ~ a, g ~ a, h ~ a, i ~ a) => Label (a, b, c, d, e, f, g, h, i) where
    labelPairs (!k1, !k2, !k3, !k4, !k5, !k6, !k7, !k8, !k9)
               (!v1, !v2, !v3, !v4, !v5, !v6, !v7, !v8, !v9) =
            LabelPairs [LabelPair k1 v1, LabelPair k2 v2, LabelPair k3 v3, LabelPair k4 v4, LabelPair k5 v5, LabelPair k6 v6,
             LabelPair k7 v7, LabelPair k8 v8, LabelPair k9 v9]
