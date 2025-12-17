# Read Filter File

Read filter settings from an Excel or CSV file and return as a
GLSFilterSettingsList object. The filter file should contain columns for
logger_id, species, colony, and various filter settings. See
`create_filter_file` for an example of the expected format.

## Usage

``` r
read_filter_file(filepath)
```

## Arguments

- filepath:

  Filepath to the filter settings file (Excel or CSV).

## Value

A GLSFilterSettingsList object containing the filter settings.
