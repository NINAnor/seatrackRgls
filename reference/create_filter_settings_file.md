# Create Filter Settings File

Create an example filter settings Excel file for specified species. The
file will contain default filter settings for each species. See
`read_filter_file` for reading the filter file.

## Usage

``` r
create_filter_settings_file(filepath, species = c())
```

## Arguments

- filepath:

  Filepath to save the filter settings Excel file.

- species:

  Vector of species names to include in the filter file. If empty,
  creates an empty template with default settings for black-legged
  kittiwake.

## Value

None. The function creates an Excel file at the specified filepath.
