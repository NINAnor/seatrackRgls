# Apply filters to loaded light data to estimate positions

Given light data, calibration data, filter settings, colony info, and
extra metadata, this function processes the light data to estimate
positions and apply various filters. For more information on each
filter, see the individual filter function help files.

## Usage

``` r
apply_filters(
  light_data,
  light_data_calibration,
  logger_filter,
  logger_colony_info,
  logger_extra_metadata,
  show_filter_plots = FALSE,
  plotting_dir = NULL,
  prev_posdata_export = NULL,
  type = "main",
  calibration_mode = FALSE
)
```

## Arguments

- light_data:

  A data frame containing the loaded light data.

- light_data_calibration:

  A data frame containing calibration data for the logger.

- logger_filter:

  A list of filter settings specific to the logger species.

- logger_colony_info:

  A data frame containing colony information for the logger.

- logger_extra_metadata:

  A data frame containing extra metadata for the logger. Defaults to
  NULL.

- show_filter_plots:

  A logical indicating whether to show filter plots. Defaults to FALSE.

- plotting_dir:

  An optional directory path to save plotting outputs. Defaults to NULL.

- prev_posdata_export:

  An optional data frame containing previous position data for
  comparison in seasonal calibration. Defaults to NULL.

- type:

  A string indicating the type of calibration: "main", "winter", or
  "summer". Defaults to "main". This setting is primarily for internal
  use during seasonal calibration. Generally users should only call this
  function with type = "main".

- calibration_mode:

  A logical indicating whether to run in calibration mode. Defaults to
  FALSE.

## Value

If calibration_mode is FALSE, returns a list containing: -
`twilight_estimates`: A data frame of twilight estimates. -
`posdata_export`: A data frame of processed position data. -
`filtering`: A data frame summarizing the filtering steps applied. If
type is `main` and calibration_mode is `FALSE`, the returned
`posdata_export` will include seasonal adjustments. If calibration_mode
is TRUE, returns data frame of default calibration outputs. If type is
not `main`, compares sun angles to previous calibration in order to
return adjusted sun angles.
