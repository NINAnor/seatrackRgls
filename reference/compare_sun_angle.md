# Compare Sun Angles to Optimize Position Estimation

This function compares sun angles from previous and new position data
exports to optimize position estimation based on latitude differences.

## Usage

``` r
compare_sun_angle(prev_posdata_export, new_posdata_export, type)
```

## Arguments

- prev_posdata_export:

  A data frame containing previous position data export with columns
  `eqfilter`, `lat`, `type`, and `date_time`.

- new_posdata_export:

  A data frame containing new position data export with columns
  `tFirst`, `tSecond`, `type`.

- type:

  A string indicating the type of sun angle sequence to use. Options are
  `"general"`, `"summer"`, or `"winter"`.

## Value

A list containing the optimal sun angles for the start and end of the
track: `sun_angle_start` and `sun_angle_end`.
