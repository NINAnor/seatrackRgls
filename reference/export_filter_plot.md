# Wrapper to export filter plots

This function wraps around a code call to export plots used in filtering
steps. If `show_filter_plots` is FALSE, the code is simply evaluated
without plotting Otherwise, if `show_filter_plots` is TRUE, the code is
evaluated and the resulting plot is saved to a PNG file in the specified
`filter_plots_dir`.

## Usage

``` r
export_filter_plot(
  call = {
 },
  show_filter_plots = TRUE,
  plot_name = "",
  logger_id_year = "",
  plotting_dir = NULL
)
```

## Arguments

- call:

  The code to be evaluated, typically a function call that generates a
  plot.

- show_filter_plots:

  Logical indicating whether to show and save filter plots.

- plot_name:

  A string specifying the name of the plot file (without extension).

- logger_id_year:

  A string to identify the logger and year, used in the plot filename.

- filter_plots_dir:

  The directory where the plot files should be saved. If NULL, plots are
  not saved to files.

## Value

The result of evaluating the provided code call.
