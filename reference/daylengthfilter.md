# Remove twilights that form subsequent outliers in length of day or night.

This function performs the following steps:

1.  Separate rows of day (type=1)/night(type=2), and calculate length of
    each day or night

2.  Fill short time gaps with baytrends::fillMissing().

3.  Fill large time gaps assuming polar night or midnight sun: Add 23.5
    hrs day/0.5hrs night (midnight sun) or 0.5 hrs day/23.5 hrs night
    (polar night) for dates without twilights, assuming summer or winter
    solstice if otherwise quite long (\>16hrs) or quite short day
    (\<8hrs) in that period of the year.

4.  Calculate different length in day/night between consecutive dates.

5.  Initial filtering: Endpoints are filtered by comparing day/night
    lengths to a 20-day mean, to accommodate the often poor data quality
    due to shading from nest attendance at start and end of logging
    periods

6.  Main filtering: daily day/night lengths compared to loess
    predictions of day/night length based on 5 day running means.

7.  Twilights that results in lengths being more than 6hrs, but less
    than 48 hour different from the previous date removed.

8.  At last, repeat all above steps, but now filter twilights that
    results in lengths being more than 3hrs, but less than 48 hour
    different from the previous date removed.

## Usage

``` r
daylengthfilter(df, show_plot)
```

## Arguments

- df:

  Input data with twilight times in GMT shown as 'tfirst' and 'tsecond'
  (yyyy-mm-dd hh:mm:ss UTC) and 'type', with being 1 (tfirst is sunrise)
  or 2 (tfirst is sunset).

- show_plot:

  if TRUE, plot is produced with day/night length and affected twilights
  highlighted

## Value

A filtered data.frame in the same format.
