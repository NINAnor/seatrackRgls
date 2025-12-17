# Prepare Calibration Data from a Folder

Convience function for preparing GLS logger data for light data
calibration. Uses
[`process_folder()`](https://ninanor.github.io/seatrackRgls/reference/process_folder.md)
in calibration mode to handle a directory of light data. See
'process_folder()' for more details. Custom settings can be provided
using `filter_setting_list`. For the creation of an empty filter
settings template see
[`create_filter_settings_file()`](https://ninanor.github.io/seatrackRgls/reference/create_filter_settings_file.md).
If `export_calibration_template` is `TRUE`, it will export an empty
calibration sheet using 'calibration_to_wb()'.

## Usage

``` r
prepare_calibration(
  import_directory,
  metadata,
  all_colony_info,
  output_directory,
  show_filter_plots = FALSE,
  export_calibration_template = TRUE,
  filter_setting_list = seatrackRgls::seatrack_settings_list
)
```

## Arguments

- import_directory:

  Directory containing raw light data files. These are expected to be
  named in the format `<logger_id>_<year_retrieved>_<logger_model>`,
  e.g. `C23_2015_mk4083`

- metadata:

  A data frame containing calibration data for all loggers or a string
  providing a filepath to read this data from. This can be from an excel
  file, CSV file or a directory containing multiple calibration files.
  This dataframe can consist of a single row per logger/year
  combination, with the following columns `logger_id`, `species`,
  `colony`, `date_deployed` and `date_retrieved`. Providing
  `logger_model` is strongly advised. These will be automatically split
  into time windows, using the `year_split` setting (defaulting to
  "01-06" in all 'seatrack_settings_list'). Alternatively, custom time
  windows can be provided by including columns `start_datetime` and
  `end_datetime`, with one time window per row.

- all_colony_info:

  A data frame containing colony information for all loggers (one row
  per colony). The required columns are `colony`, `latitude`, and
  `longitude`.

- output_directory:

  Directory to save calibration templates and calibration plots. If
  `show_filter_plots` is `TRUE`

- show_filter_plots:

  A logical indicating whether to show individual filter plots for the
  default sun angle. Defaults to `FALSE`.

- export_calibration_template:

  When `TRUE` will export a calibration template in excel format.
  Otherwise returns the calibration template as an R object. Defaults to
  `TRUE`.

- filter_setting_list:

  A 'GLSFilterSettingsList' object containing filter settings for
  loggers or a path to load one using
  [`read_filter_file()`](https://ninanor.github.io/seatrackRgls/reference/read_filter_file.md).
  Defaults to 'seatrackRgls::seatrack_settings_list'.

## Details

Will export calibration plots to assist in choosing appropriate sun
angles for processing GLS logger data (see vignette). Once these are
decided on, final positions can be exported using process_positions()
