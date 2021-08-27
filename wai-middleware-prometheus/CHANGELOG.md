# Change Log

## 1.0.0.1

- Increase allowed prometheus-client version ranges.

## 1.0.0

- Supports prometheus-client-1.0.0

## 0.3.0

## 0.2.0

- Time measurments are now exported as seconds instead of microseconds (#17).
- Histograms are used instead of summaries for time measurments. This allows for
  aggregration across multiple processes (#18).

## 0.1.1

