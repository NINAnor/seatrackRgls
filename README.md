
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seatrackRgls

<!-- badges: start -->

<!-- badges: end -->

The goal of seatrackRgls is to estimate up to two positions a day from
light-level data from geolocators (Lotek, BAS, Biotrack, Migrate
Technology) using a threshold method. It automatically edits twilight
events used for calculating positions and filter positions based on
flight speed, distribution, distance, and midnight sun periods. If
allowed, the package saves plots to your local computer the aid a
subjective calibration of sun elevation angles, which is crucial for
producing realistic latitudes. The script should be run twice. First
with a “best-guess” sun angle, then with the calibrated sun angle.

## Installation

You can install the development version of seatrackRgls from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("NINAnor/seatrack-gls")
```

## Example

TODO

``` r
library(seatrackRgls)
## basic example code
```
