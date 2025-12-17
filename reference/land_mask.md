# Land mask

The function measures the distance between any location and to a
coastline, using the package ‘distancetocoast’ and a high-resolution
coastline from Natural Earth ("10"). Land and ocean location can be
removed surpassing a specified distance in km. To avoid this function,
apply very far (e.g '10000' km) or infinite ('Inf') distance.

## Usage

``` r
land_mask(lon, lat, coast_to_land, coast_to_sea, eqfilter)
```

## Arguments

- lon:

  longitude in decimal degrees

- lat:

  latitude in decimal degrees

- coast_to_land:

  locations filtered if X km inland from coastline

- coast_to_sea:

  locations filtered if X km offshore from the coastline

- eqfilter:

  if equinox, locations are not filtered

## Value

A data frame with longitude and latitude filtered.
