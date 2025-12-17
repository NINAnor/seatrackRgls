# Edit twilights by location-dependent thresholds.

This filter identify and act on false twilights that fulfil two
conditions: first, the twilight must be clearly different from its
adjacent twilights of the same type (e.g sunsets), and second, both
adjacent twilights must occur at a similar time of day. If these
conditions are fulfilled, the time of day for the identified twilight is
changed to the average of both adjacent twilights. The filter will have
no effect in periods where the variation in twilight timing is high,
either due to migration, or the unfortunate influence of shadow and
artificial light is present over several days.

## Usage

``` r
move_twilights(df, speed, sun, show_plot)
```

## Arguments

- df:

  Input data with tFirst, tSecond, type

- speed:

  Maximum expected movement rate as km/h, sustained between two
  locations with ~12 hours in between. Second location in a pair will be
  removed.

- sun:

  calibrated sun angles per date

- show_plot:

  TRUE or FALSE

## Value

A data frame with edited twilights.

## Details

This function performs the following steps:

1.  Make time circular.

2.  Make a reference table for selecting thresholds (daily change of
    light per lat/month in min\*2 + 15m variance).

3.  Define the threshold in minutes for which A and C is considered the
    same (twilight A will define the threshold):

4.  For this we use the reference table to select the right month and
    which reference latitude that is closest to the birds' approx.
    latitude

5.  During an approx equinox period the function use estimated lats
    based on 10 lats before and after equinox period

6.  for a few remaining NA's, the average threshold across lats and
    months is used (22 min)

7.  Threshold for B different from A and C: minutes_different \<- (speed
    / 24) \* 8 (20 min is minimum) + rate of change in daylight

8.  Produce plot with affected twilights highlighted
