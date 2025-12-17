# Loess Filter for Position Data

Applies a loess filter to position data if the track length exceeds 45
days.

## Usage

``` r
loess_filter(posdata, logger_filter)
```

## Arguments

- posdata:

  A data frame containing position data with columns `tFirst`,
  `tSecond`, and `type`.

- logger_filter:

  A list containing logger filter parameters, including \`loess_filter_k

## Value

A data frame with loess-filtered positions if applicable; otherwise, the
original data frame.
