# Package index

## Data calibration and position export

High level functions for the processing of GLS light data

- [`prepare_calibration()`](https://ninanor.github.io/seatrackRgls/reference/prepare_calibration.md)
  : Prepare Calibration Data from a Folder
- [`process_positions()`](https://ninanor.github.io/seatrackRgls/reference/process_positions.md)
  : Process Positions from a Folder

## Filters

Filters used to clean light data while processing.

- [`argos_filter()`](https://ninanor.github.io/seatrackRgls/reference/argos_filter.md)
  : Argos Filter for Position Data
- [`aut_midnight_sun_removal()`](https://ninanor.github.io/seatrackRgls/reference/aut_midnight_sun_removal.md)
  : Remove false positions during breeding or midnight sun periods
- [`bounding_box_filter()`](https://ninanor.github.io/seatrackRgls/reference/bounding_box_filter.md)
  : Bounding Box Filter for Position Data
- [`daylengthfilter()`](https://ninanor.github.io/seatrackRgls/reference/daylengthfilter.md)
  : Remove twilights that form subsequent outliers in length of day or
  night.
- [`double_smoothing()`](https://ninanor.github.io/seatrackRgls/reference/double_smoothing.md)
  : Double smoothing of positions
- [`equinox_filter()`](https://ninanor.github.io/seatrackRgls/reference/equinox_filter.md)
  : Equinox Filter for Position Data
- [`land_mask()`](https://ninanor.github.io/seatrackRgls/reference/land_mask.md)
  : Land mask
- [`loess_filter()`](https://ninanor.github.io/seatrackRgls/reference/loess_filter.md)
  : Loess Filter for Position Data
- [`move_twilights()`](https://ninanor.github.io/seatrackRgls/reference/move_twilights.md)
  : Edit twilights by location-dependent thresholds.
- [`noonfilter()`](https://ninanor.github.io/seatrackRgls/reference/noonfilter.md)
  : Remove twilights that form subsequent outliers in time of noon and
  midnight.
- [`speed_filter()`](https://ninanor.github.io/seatrackRgls/reference/speed_filter.md)
  : Speed Filter for Position Data
- [`twilight_cleanup()`](https://ninanor.github.io/seatrackRgls/reference/twilight_cleanup.md)
  : Estimate and Refine Locations From Light Data Recorded by
  Geolocators.

## Processing

Functions for the bulk processing of GLS light data

- [`process_folder()`](https://ninanor.github.io/seatrackRgls/reference/process_folder.md)
  : Process all light position files in a folder
- [`process_logger_year()`](https://ninanor.github.io/seatrackRgls/reference/process_logger_year.md)
  : Prepare light position files a single logger and year combination
- [`process_logger_light_data()`](https://ninanor.github.io/seatrackRgls/reference/process_logger_light_data.md)
  : Load and process a logger/year combination's light files
- [`apply_filters()`](https://ninanor.github.io/seatrackRgls/reference/apply_filters.md)
  : Apply filters to loaded light data to estimate positions

## Data preparation

Functions used while preparing light data for processing

- [`assign_equinox_periods()`](https://ninanor.github.io/seatrackRgls/reference/assign_equinox_periods.md)
  : Assign equinox periods
- [`twilight_estimation()`](https://ninanor.github.io/seatrackRgls/reference/twilight_estimation.md)
  : Estimate twilight times from light data

## Custom filter settings

Tools to provide custom filter settings

- [`GLSFilterSettingsList`](https://ninanor.github.io/seatrackRgls/reference/GLSFilterSettingsList.md)
  : GLSFilterSettingsList
- [`GLSsettings`](https://ninanor.github.io/seatrackRgls/reference/GLSsettings.md)
  : GLSsettings Class
- [`create_filter_settings_file()`](https://ninanor.github.io/seatrackRgls/reference/create_filter_settings_file.md)
  : Create Filter Settings File
- [`read_filter_file()`](https://ninanor.github.io/seatrackRgls/reference/read_filter_file.md)
  : Read Filter File

## Data

Data used in this package

- [`equinox_table`](https://ninanor.github.io/seatrackRgls/reference/equinox_table.md)
  : Equinox table
- [`example_metadata`](https://ninanor.github.io/seatrackRgls/reference/example_data.md)
  [`example_colony_info`](https://ninanor.github.io/seatrackRgls/reference/example_data.md)
  [`example_extra_metadata`](https://ninanor.github.io/seatrackRgls/reference/example_data.md)
  : Example metadata for GLS loggers A dataset containing example
  metadata for GLS loggers.
- [`seatrack_settings_list`](https://ninanor.github.io/seatrackRgls/reference/seatrack_settings_list.md)
  : Seatrack filter settings

## Plotting

Functions used for plotting

- [`plot_a_map()`](https://ninanor.github.io/seatrackRgls/reference/plot_a_map.md)
  : Produce a Quick Map

## General functions

General utility functions

- [`calibration_to_wb()`](https://ninanor.github.io/seatrackRgls/reference/calibration_to_wb.md)
  : Write Calibration Data to Excel Workbook
