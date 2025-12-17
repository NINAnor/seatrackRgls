# Assign equinox periods

Assign which days of the year estimated latitudes are unreliable due to
equinox. Limits are based on

1.  Which approximate latitude did the geolocator record light-data
    (objective; towards the poles changes in daylength is more dramatic
    around equinox than at equator = shorter equinox period).

2.  how much uncertainty allowed (subjective) : uncertainty gets extreme
    right up to the equinox and less further away in time. ut off point
    of the equinox period aim to keep an average day-to-day variance in
    latitude below 1.5 degrees (165 km).

3.  Results from sun angle calibrations (objective) which is a result
    of a) threshold (light-level) used to differentiate between night
    and daytime and b) the sensor's light sensitivity.

## Usage

``` r
assign_equinox_periods(lats, dates, breedingloc_lat, sun)
```

## Arguments

- lats:

  Numeric value with latitudes from the tracked individual.

- dates:

  Dates (tfirst) when latitudes occur

- breedingloc_lat:

  Approximate latitude for the breeding locality.

- sun:

  Numeric value showing the sun's angle to the horizon at each twilight.
  Is a result based on the chosen threshold of light used in twilight
  estimations and the light sensor sensitivity

## Value

A list saying if the equinox effect is present or not (1 = no equinox
effect, 0 = equinox effect).

## Details

Point 1-3 are compared to a reference table:

Apparent equinox (period midpoint) is defined from how sun angles
compare to calculations in Hill & Braun (2001). Period start and end is
based on estimations of variance in Hill & Braun (2001) and the mean
day-to-day variance in latitudes observed in 16 000 tracked non-breeding
seasons in SEATRACK.

The function performs the following steps:

1.  Calculate median sun angle for a 5-day period at yday 41:45 (\>30
    days before spring EQ), yday 113:117 (\>30 days after spring EQ),
    yday 227:231 (\>30 days before autumn EQ), yday 300:304 (\>30 days
    after autumn EQ).

2.  If no sun angles available - use first sun angle

3.  Calculate median latitudes for a 5-day period at yday 41:45 (\>30
    days before spring EQ), yday 113:117 (\>30 days after spring EQ),
    yday 227:231 (\>30 days before autumn EQ), yday 300:304 (\>30 days
    after autumn EQ).

4.  If no latitudes because of midnight sun - use breeding location

5.  Matches e.g yday 41:45 median sun angle and median latitude to the
    reference table within the R package to identify the start of spring
    equinox period an so on.
