# Remove twilights that form subsequent outliers in time of noon and midnight.

This function performs the following steps:

1.  Calculate noon/midnight based on twilight.

2.  make noon/midnight circular.

3.  split data set into day and night

4.  loess predictions of noon/midnight (after excluding twilights with
    extreme variation from predictions)

5.  fill data gaps in predictions with baytrends::fillMissing()

6.  find the minimum difference between a noon/midnight directly
    calculated from twilights and any predicted noon/midnight 4 days
    ahead or after candidate noon/midnight.

7.  threshold for removing twilight is when difference between
    noon/midnight vs prediction overreach 0.4h, but under under 12h

## Usage

``` r
noonfilter(df, show_plot)
```

## Arguments

- df:

  Input data with twilight times in GMT shown as 'tfirst' and 'tsecond'
  (yyyy-mm-dd hh:mm:ss UTC) and 'type', with being 1 (tfirst is sunrise)
  or 2 (tfirst is sunset).

- show_plot:

  if TRUE, plot is produced with timing of noon/midnight and affected
  twilights highlighted

## Value

A filtered data.frame in the same format.
