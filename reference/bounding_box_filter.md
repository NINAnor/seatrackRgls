# Bounding Box Filter for Position Data

Filters position data based on a defined bounding box, removing
positions outside the specified latitude and longitude limits.

## Usage

``` r
bounding_box_filter(posdata, light_data_calibration, logger_filter)
```

## Arguments

- posdata:

  A data frame containing position data with columns `lon`, `lat`, and
  `tFirst`.

- light_data_calibration:

  A list containing light data calibration parameters.

- logger_filter:

  A list containing logger filter parameters, including default boundary
  box limits.

## Value

A filtered data frame with positions outside the bounding box removed.
