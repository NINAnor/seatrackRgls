# Prepare light position files a single logger and year combination

Given a logger ID and year, this function processes all light data files
for that logger/year combination, finding the appropriate light files in
the specified import directory and applying calibration data, filter
settings, colony info, and extra metadata as needed. These are then
passed to 'process_logger_light_data()' to handle loading the light data
and further processing.

## Usage

``` r
process_logger_year(
  logger_id,
  year,
  import_directory,
  calibration_data,
  all_colony_info,
  filter_setting_list = seatrackRgls::seatrack_settings_list,
  extra_metadata = NULL,
  show_filter_plots = FALSE,
  export_maps = TRUE,
  plotting_dir = NULL,
  output_dir = NULL,
  calibration_mode = TRUE,
  analyzer = ""
)
```

## Arguments

- logger_id:

  The ID of the logger to process.

- year:

  The `year_retrieved` to process for the logger.

- import_directory:

  Directory containing raw light data files. These are expected to be
  named in the format `<logger_id>_<year_retrieved>_<logger_model>`,
  e.g. `C23_2015_mk4083`

- calibration_data:

  A data frame containing metadata/calibration data for all loggers or a
  string providing a filepath to read this data from. This can be from
  an excel file, CSV file or a directory containing multiple calibration
  files. In calibration mode, this dataframe can consist of a single row
  per logger/year combination. In calibration mode, the minimum required
  columns are `logger_id`, `species`, `colony`, `date_deployed` and
  `date_retrieved`. Providing `logger_model` is strongly advised. If not
  in calibration mode, the data frame is expected to be in the format
  output by running this function in calibration mode.

- all_colony_info:

  A data frame containing colony information for all loggers (one row
  per colony). The required columns are `colony`, `latitude`, and
  `longitude`.

- filter_setting_list:

  A 'GLSFilterSettingsList' object containing filter settings for
  loggers or a path to load one using 'read_filter_file()'. Defaults to
  'seatrack_settings_list'.

- extra_metadata:

  Optional data frame containing extra metadata for loggers. This must
  have the column `logger_id` to join extra metadata. A `year_retrieved`
  column can also be provided to join based on a combination of logger
  and which session this is.

- export_maps:

  A logical indicating whether to export maps of the processed
  positions. Defaults to `TRUE`.

- plotting_dir:

  An optional directory path to save plotting outputs. Defaults to
  `NULL`.

- output_dir:

  An optional directory path to save processed outputs. Defaults to
  `NULL`.

- calibration_mode:

  A logical indicating whether to run in calibration mode. Defaults to
  `TRUE`

- analyzer:

  An optional string indicating the analyzer who provided calibration
  data, if this column is not provided in calibration data. Defaults to
  an empty string.

## Value

If calibration_mode is `FALSE`, returns a list containing: -
`twilight_estimates`: A data frame of twilight estimates. -
`posdata_export`: A data frame of processed position data. -
`filtering`: A data frame summarizing the filtering steps applied. If
calibration_mode is `TRUE`, returns data frame of default calibration
outputs and (if plottinging dir is not `NULL`) exports calibration
plots.
