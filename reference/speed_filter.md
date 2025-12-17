# Speed Filter for Position Data

Filters position data based on travel speed and longitude changes during
equinox periods.

## Usage

``` r
speed_filter(posdata, speed)
```

## Arguments

- posdata:

  A data frame containing position data with columns `lat_smooth2`,
  `lon_smooth2`, `tFirst`, and `eqfilter`.

- speed:

  Maximum allowable speed in km/h.

## Value

A filtered data frame with positions exceeding speed limits or showing
abnormal longitude changes during equinox periods removed.
