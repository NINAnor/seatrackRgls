# Estimate and Refine Locations From Light Data Recorded by Geolocators.

Filter that remove false days and nights caused by recording shading or
artificial light patterns that produce errornous twilights, The filter
is made to identify dates with an impossible number of twilights.
Normally, there is one sunset and one sunrise to a date, but to not
affect long-range migration, the filter allow dates with two sunsets or
two sunrises as long as sunsets or sunrises occur more than 22 hours
apart from each other. The function then predict timing of twilights to
be used for selecting the most likely set of twilights within these
dates. It use predicted times to avoid referring to singular datapoints
that very well can be outliers.

## Usage

``` r
twilight_cleanup(
  df,
  breedingloc_lon,
  breedingloc_lat,
  months_breeding,
  species,
  show_plot,
  sun_angle_start,
  sun_angle_end,
  show_filter_plots = FALSE
)
```

## Arguments

- df:

  data.frame with 'tFirst', 'tSecond', 'type','sun'.

- breedingloc_lon:

  longitude where bird was instrumented informs the algorithm

- breedingloc_lat:

  latitude where bird was instrumented informs the algorithm

- months_breeding:

  Expected months with regular presence at breeding location

- species:

  default NA, information can be added for convenience

- show_plot:

  TRUE/FALSE default NA, information can be added for convenience

- sun_angle_start:

  if NA, default is -3.5. Analyser to add manually calibrated value
  after an initial run of the function

- sun_angle_end:

  filled if compensating for a change in light sensitivity (default is
  NA)

## Value

A data frame with raw and smoothed locations, twilight time, threshold
and sun angle used to estimate locations, as well as some convenient
info about logger and individual.

## Details

This function performs the following steps:

1.  find dates with too many twilights (1 sunset and 1 sunrise per 22
    hours)

2.  Build predictions of time of sunset and sunrise by keeping unlikely
    times out of predictions with loess filter and std deviation.

3.  Standard deviation: calculate SD every 5th day to keep points with
    SD\> 60mins out when making predictions

4.  Fill predictions for dates with no light with twilights that makes
    up a daylength of 0 hours.

5.  Retain the candidate twilight that is closest to a predicted
    twilight, remove the others
