# Investigating Memory Use in `prometheus-client`

We're running into problems with unexpectedly high memory use in `prometheus-client`, so I've been investigating improving the behavior.

Specifically, we're notificing a significant increase in the amount of time and memory allocated while calling `exportMetricsToText`.
Every time we call the endpoint, more and more memory is used to render the response.
There are two reasonable possibilities for this:

1. The `exportMetricsToText` is producing a significantly larger `Text` value, which naturally requires significantly more memory.
2. The metrics themselves are somehow holding on to excessive memory or thunks.

Diagnosing this in our application is difficult - our profiling build is currently broken.
So I'm currently just looking at the code and correcting known smells.

## `LabelPairs`

The `LabelPairs` type was a `[(Text, Text)]`.
Lists and tuples are both known sources of potential laziness issues.
As a first pass, I replaced the type with a `newtype` so I could control the API for accessing and adding entries.
Then I replaced the tuple with a `data LabelPair = LabelPair { labelKey :: !Text, labelValue :: !Text }`.
This should prevent thunk accumulation for labels, and the concrete type may enable further memory improvement from GHC.

The fields on `Sample` are made strict, as well.
This should prevent thunk accumulation on the `Text` and `ByteString`, but a lazy `LabelPairs` is still possible.

Additionally, the `labelPairs` function now uses bang patterns on each `Text` in the tuple.
Since this is the whole of the interface for constructing a `LabelPair`, this should prevent any thunk accumulation on labels.

## `MetricImpl`

A `Metric` had a field `construct :: IO (s, IO [SampleGroup])`.
To avoid tuple, we introduce `MetricImpl s` which uses bang patterns on the fields.

In practice, the `MetricImpl s` is almost always instantiated to a reference type, and evaluating a reference doesn't do much to help.
I did find the clarity in names helpful.

## `VectorState`

A `Prometheus.Metric.Vector` previously was defined as:

```haskell
type VectorState l m = (Metric m, Map.Map l (m, IO [SampleGroup]))
```

This `VectorState` was being stored in an `IORef`.
While the operation was evaluating the `VectorState` to WHNF, this only evaluated the tuple constructor, leaving the `Metric` and `Map.Map` unevaluated.
The `Map` is from `Data.Map.Strict`, which means that the values are evaluated to WHNF.
Methods from that module evaluate the structure of the `Map`, but polymorphic methods (ie `Functor`, `Traversable`) *do not*.

We can reduce the possibility of memory leaks here by replacing this with a record, bang patterns, and omitting the tuple for the `MetricImpl` type.

## `Counter` to `Builder`

`Counter` is a very simple metric.
Since the operations used for modifying the `IORef` all evaluate the value to WHNF, and the value is a `Double`, we are not retaining memory here.

However, there is a performance inefficiency: the use

```haskell
    let sample = Sample (metricName info) mempty (BS.fromString $ show value)
```

`show :: Double -> String` is going to inefficiently allocate a `[Char]`, which will then be packed using `BS.fromString`.
Later, in consumption of this, we will convert that `ByteString` into a `Builder`.
A more efficient approach would use [`doubleDec`](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Builder.html#v:doubleDec) to directly convert to a `Builder`, avoiding allocating the intermediate `String`.

Since the only actual use of the `Sample`'s payload value is in building the report, we can change `Sample` to contain a `Builder` and encode things directly.
This will improve efficiency by avoiding allocating intermediate `String`s.

## `Histogram`

While investigating `Histogram`, I found a few potential issues:

1. `cumulativeSum` has numerous problems: 
    * The function holds onto the entire `Map` converted-into-a-`[(Double, Int)]` in order to `zip` it with itself. 
    * `scanl1` is lazy, similar to `foldl`. On lists, this will result in thunk accumulation.
2. `showFFloat` is used, requiring a `Double -> String -> Text -> Builder` conversion path.
3. The entire computation is done inside of a `STM.atomically`.
   This means that, should anything write to the `TVar`, all of the computation will be retried.
   This is *probably* bad - we want to capture the state of the metrics *now*, and then return the `SampleGroup`, rather than allowing the computation to be aborted and retried multiple times.

The first two problems are based on the sizeof the histogram, so the number of buckets.
Every additional bucket causes another float to be rendered into a string, and another list cons cell to be held on to.
Since number of buckets is likely small, this is probably not a big deal.

However, might as well fix it up where I can!
`scanl1'` does not exist in `base`, but we can avoid retaining the input list in memory by preserving the tuple structure.
A bang pattern on the accumulator can help.

`formatFloat` is used to produce a `Text` label value for the `LabelPair`.
This suggests we can use `Data.Text.Lazy.Builder` to avoid the intermediate `String`.
