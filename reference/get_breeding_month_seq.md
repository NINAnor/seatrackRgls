# Generate a sequence of breeding months

This function generates a sequence of months representing the breeding
period, taking into account cases where the breeding period spans the
end of the year.

## Usage

``` r
get_breeding_month_seq(start_month = 6, end_month = 8)
```

## Arguments

- start_month:

  An integer representing the starting month of the breeding period
  (1-12).

- end_month:

  An integer representing the ending month of the breeding period
  (1-12).

## Value

A vector of integers representing the months in the breeding period.
