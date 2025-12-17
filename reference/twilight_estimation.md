# Estimate twilight times from light data

\#' This function estimates twilight times (sunrise and sunset) from
light data using the `twilightCalc_bugfree` function. It also provides
an option to visualize the filtering process through plots.

## Usage

``` r
twilight_estimation(
  light_data,
  light_data_calibration,
  show_filter_plots = FALSE
)
```

## Arguments

- light_data:

  A dataframe containing light data with columns 'dtime' (datetime) and
  'lux' (light intensity).

- light_data_calibration:

  A list containing calibration parameters, including 'light_threshold'
  and 'logger_model'.

- show_filter_plots:

  A logical value indicating whether to display plots of the filtering
  process. Default is FALSE.

## Value

a dataframe where the first two columns indicate timing of twilight
events and a column type, where type == 1 indicates that tFirst is
sunrise and type == 2 indicates that tFirst is sunset.
