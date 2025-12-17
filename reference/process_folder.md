# Process all light position files in a folder

Given an import directory containing light data files this function
processes all light data files in the folder. It applies calibration
data, filter settings, colony info, and extra metadata as needed for
each logger/year combination found in the folder using
'process_logger_year()' In calibration mode, it can export a combined
calibration data file for all processed loggers. If not in calibration
mode, it exports processed position data, filtering summaries, and
twilight estimates for each logger/year combination.

## Usage

``` r
process_folder(
  import_directory,
  calibration_data,
  all_colony_info,
  filter_setting_list = seatrackRgls::seatrack_settings_list,
  extra_metadata = NULL,
  show_filter_plots = FALSE,
  output_dir = NULL,
  calibration_mode = TRUE,
  export_calibration_template = TRUE
)
```

## Arguments

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

- show_filter_plots:

  A logical indicating whether to show filter plots. Defaults to
  `FALSE`.

- output_dir:

  An optional directory path to save processed outputs. Defaults to
  `NULL`.

- calibration_mode:

  A logical indicating whether to run in calibration mode. Defaults to
  `TRUE`. If `TRUE`, the function can export or return a combined
  calibration data file for all logger/year combinations. If `FALSE`,
  the function exports processed position data, filtering summaries, and
  twilight estimates for each logger/year combination.

- export_calibration_template:

  A logical indicating whether to export an excel calibration template.
  Defaults to `TRUE`. If `FALSE`, the calibration template is returned.
