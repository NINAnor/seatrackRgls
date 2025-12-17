# Double smoothing of positions

The raw positions have relatively low precision (Phillips et al. 2004)
and also exhibit a typical noon–midnight zigzag pattern in latitude due
to east–west movements, and to lesser extent in longitude due to north –
south movements (Fox 2010, 2015). To reduce the influence of inaccurate
positions and compensate for movements a double smoothing procedure
described in Hanssen et al. 2016 can be applied (DOI:
10.1007/s00300-016-1908-z).

## Usage

``` r
double_smoothing(df, sun)
```

## Arguments

- df:

  data.frame with twilight times as 'tFirst' and 'tSecond' and 'type' (1
  = tFirst is morning, 2 = tFirst is evening)

- sun:

  corresponding sun angle for each latitude and longitude

## Value

A data frame with six additional columns; 'lat' & 'lon', 'lat_smooth1' &
'lon_smooth1' and 'lat_smooth2' & 'lat_smooth2'.

## Details

This function performs the following steps:

1.  First smoothing, essentially compensation for east-west movements
    (mean lat (lat_smooth1)) and north-south movements (mean lon
    (lon_smooth1)), noon-midnight.

2.  Compensate for positions crossing the Pacific 180 Meridian

3.  Second smoothing, two point moving average of noon-midnight
    positions ('lat_smooth2' and 'lon_smooth2'), same as Phillips et
    al.2004
