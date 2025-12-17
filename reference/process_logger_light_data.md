# Load and process a logger/year combination's light files

Given a set of filepaths for light data files, calibration data, filter
settings, colony info and extra metadata, this function processes the
light data to estimate positions and apply various filters depending on
whether calibration mode is active or not.

## Usage

``` r
process_logger_light_data(
  filepaths,
  logger_calibration_data,
  filter_setting_list,
  logger_colony_info,
  logger_extra_metadata = NULL,
  show_filter_plots = FALSE,
  plotting_dir = NULL,
  calibration_mode = TRUE,
  min_length = 40
)
```

## Arguments

- filepaths:

  A vector of file paths to the light data files.

- logger_calibration_data:

  A data frame containing calibration data for the logger. If multiple
  calibration windows are provided, each will be processed in sequence.

- filter_setting_list:

  A list of filter settings for different species. Defaults to
  `seatrack_settings_list`.

- logger_colony_info:

  A data frame containing colony information for the logger.

- logger_extra_metadata:

  A data frame containing extra metadata for the logger.

- show_filter_plots:

  A logical indicating whether to show filter plots. Defaults to FALSE.

- plotting_dir:

  An optional directory path to save plotting outputs. Defaults to NULL.

- calibration_mode:

  A logical indicating whether to run in calibration mode. Defaults to
  TRUE.

- min_length:

  Number indicating minimum length of light data. Anything below this
  will fail. Defaults to 40.

## Value

If calibration_mode is FALSE, returns a list containing: -
`twilight_estimates`: A data frame of twilight estimates. -
`posdata_export`: A data frame of processed position data. -
`filtering`: A data frame summarizing the filtering steps applied. If
calibration_mode is TRUE, returns data frame of default calibration
outputs and exports calibration plots.
