# Limit light data to calibration time windows

Limit light data to calibration time windows

## Usage

``` r
limit_light_data(light_data, logger_calibration_data)
```

## Arguments

- light_data:

  Data frame containing light data with a 'dtime' column.

- logger_calibration_data:

  Data frame containing calibration data with 'start_datetime' and
  'end_datetime' columns.

## Value

A list of data frames, each containing light data limited to the
corresponding calibration time window.
