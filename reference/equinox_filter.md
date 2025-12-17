# Equinox Filter for Position Data

Applies an equinox filter to position data based on calibrated light
data.

## Usage

``` r
equinox_filter(
  posdata,
  posdata_lat,
  light_data_calibration,
  logger_colony_info
)
```

## Arguments

- posdata:

  A data frame containing position data with columns `date_time`.

- posdata_lat:

  A numeric vector of latitudes corresponding to the position data.

- light_data_calibration:

  A list containing light data calibration parameters, including equinox
  start and end dates.

- logger_colony_info:

  A list containing logger colony information, including `col_lat`.

## Value

A data frame with an additional column `eqfilter` indicating equinox
effect presence (TRUE = no equinox effect, FALSE = equinox effect).
