module Prometheus.Label (
    Label (..)
,   LabelPairs
) where


type LabelPairs = [(String, String)]

class Ord l => Label l where
    labelPairs :: l -> l -> LabelPairs

instance Label () where
    labelPairs () () = []

instance Label String where
    labelPairs key value = [(key, value)]

instance Label (String, String)  where
    labelPairs (k1, k2) (v1, v2) = [(k1, v1), (k2, v2)]

instance Label (String, String, String)  where
    labelPairs (k1, k2, k3) (v1, v2, v3) = [(k1, v1), (k2, v2), (k3, v3)]

instance Label (String, String, String, String)  where
    labelPairs (k1, k2, k3, k4) (v1, v2, v3, v4) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4)]

instance Label (String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5) (v1, v2, v3, v4, v5) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)]

instance Label (String, String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6) (v1, v2, v3, v4, v5, v6) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6)]

instance Label (String, String, String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7) (v1, v2, v3, v4, v5, v6, v7) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7)]

instance Label (String, String, String, String, String, String, String,
                String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8)
               (v1, v2, v3, v4, v5, v6, v7, v8) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8)]

instance Label (String, String, String, String, String, String, String,
                String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9)]
