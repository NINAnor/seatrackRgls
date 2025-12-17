# Write Calibration Data to Excel Workbook

Writes the provided calibration data frame to an Excel workbook with
formatting.

## Usage

``` r
calibration_to_wb(
  all_calibration,
  calibration_output_dir,
  calibration_filename = paste0("calibration_", Sys.Date(), ".xlsx")
)
```

## Arguments

- all_calibration:

  Data frame containing calibration data for all loggers.

- calibration_output_dir:

  Directory to save the calibration Excel workbook.

- calibration_filename:

  Name of the calibration Excel file. Defaults to
  "calibration\_\<current_date\>.xlsx".

## Value

None. Saves the calibration data to an Excel workbook at the specified
file path.
