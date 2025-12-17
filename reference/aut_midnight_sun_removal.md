# Remove false positions during breeding or midnight sun periods

At the head or tail of a yearly track (e.g breedingY to breedingY+1) :
removes the first or last positions that are more than 25 hours apart in
time from the other positions. Datagaps of \>25 hours in the midst of
the track won't be assessed or filtered.

## Usage

``` r
aut_midnight_sun_removal(df)
```

## Arguments

- df:

  data.frame where 'tFirst' and 'tSecond' (both date_time) must be part
  of the first eight columns

## Value

A data.frame where non-consecutive positions have been removed from the
head and tail of the original data.frame
