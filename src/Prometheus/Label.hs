module Prometheus.Label (
    Label (..)
,   LabelPairs
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


type LabelPairs = [(String, String)]

class Ord l => Label l where
    labelPairs :: l -> l -> LabelPairs

type Label0 = ()

instance Label () where
    labelPairs () () = []

type Label1 = String

instance Label String where
    labelPairs key value = [(key, value)]

type Label2 = (String, String)

instance Label (String, String)  where
    labelPairs (k1, k2) (v1, v2) = [(k1, v1), (k2, v2)]

type Label3 = (String, String, String)

instance Label (String, String, String)  where
    labelPairs (k1, k2, k3) (v1, v2, v3) = [(k1, v1), (k2, v2), (k3, v3)]

type Label4 = (String, String, String, String)

instance Label (String, String, String, String)  where
    labelPairs (k1, k2, k3, k4) (v1, v2, v3, v4) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4)]

type Label5 = (String, String, String, String, String)

instance Label (String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5) (v1, v2, v3, v4, v5) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)]

type Label6 = (String, String, String, String, String, String)

instance Label (String, String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6) (v1, v2, v3, v4, v5, v6) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6)]

type Label7 = (String, String, String, String, String, String, String)

instance Label (String, String, String, String, String, String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7) (v1, v2, v3, v4, v5, v6, v7) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7)]

type Label8 = (String, String, String, String, String, String, String, String)

instance Label (String, String, String, String, String, String, String,
                String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8)
               (v1, v2, v3, v4, v5, v6, v7, v8) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8)]

type Label9 = (String, String, String, String, String, String, String, String,
               String)

instance Label (String, String, String, String, String, String, String,
                String, String)  where
    labelPairs (k1, k2, k3, k4, k5, k6, k7, k8, k9)
               (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
            [(k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5), (k6, v6),
             (k7, v7), (k8, v8), (k9, v9)]
