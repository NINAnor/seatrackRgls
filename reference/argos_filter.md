# Argos Filter for Position Data

Filters position data using the Argos SDA filter, removing positions
classified as "removed" or "end_location", while considering equinox
periods.

## Usage

``` r
argos_filter(
  posdata,
  light_data_calibration,
  logger_colony_info,
  logger_filter
)
```

## Arguments

- posdata:

  A data frame containing position data with columns `lat_smooth2`,
  `lon_smooth2`, `date_time`, `type`, and `eqfilter`.

- light_data_calibration:

  A list containing light data calibration parameters.

- logger_colony_info:

  A list containing logger colony information, including `col_lat` and
  `col_lon`.

- logger_filter:

  A list containing logger filter parameters, including `speed`.

## Value

A filtered data frame with positions classified as "removed" or
"end_location" by the Argos SDA filter removed, except during equinox
periods.
