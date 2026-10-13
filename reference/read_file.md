# Read a ride file

Reads a `.fit`, `.gpx` or `.csv` file into the canonical ride schema, so
nothing downstream has to care which format a ride arrived in.

## Usage

``` r
read_file(path)
```

## Arguments

- path:

  Path or URL of a `.fit`, `.gpx` or `.csv` file. A CSV needs a `time`
  column, in ISO 8601 or epoch seconds, and a `power` column.

## Value

A tibble with `time` (POSIXct, UTC) and `power` (watts), plus whichever
of `cadence`, `heart_rate`, `speed`, `distance`, `altitude`,
`temperature`, `lat` and `lon` the file carries, ordered by time.

## See also

Other reading:
[`read_rides()`](https://jzuccollo.github.io/cyclingpower/reference/read_rides.md),
[`ride_spec_from_names()`](https://jzuccollo.github.io/cyclingpower/reference/ride_spec_from_names.md)

## Examples

``` r
read_file(system.file("extdata", "power_strava.gpx", package = "cyclingpower"))
#> # A tibble: 3 × 8
#>   time                power cadence heart_rate altitude temperature   lat    lon
#>   <dttm>              <dbl>   <dbl>      <dbl>    <dbl>       <dbl> <dbl>  <dbl>
#> 1 2024-03-01 09:00:00   210      88        132     12.4          18  51.5 -0.125
#> 2 2024-03-01 09:00:01   224      89        133     12.6          18  51.5 -0.125
#> 3 2024-03-01 09:00:02   197      87        134     12.9          18  51.5 -0.125

# FIT needs FITfileR, which is not on CRAN
read_file(system.file("extdata", "power1.fit", package = "cyclingpower"))
#> # A tibble: 3,601 × 6
#>    time                power cadence heart_rate speed distance
#>    <dttm>              <dbl>   <dbl>      <dbl> <dbl>    <dbl>
#>  1 2020-11-15 19:03:24   193      87         76  3.69     3.69
#>  2 2020-11-15 19:03:25   207      86         76  4.28     7.97
#>  3 2020-11-15 19:03:26   218      88         74  4.86    12.8 
#>  4 2020-11-15 19:03:27   154      89         73  5.44    18.3 
#>  5 2020-11-15 19:03:28   171      91         72  6.06    24.3 
#>  6 2020-11-15 19:03:29   149      91         72  6.67    31   
#>  7 2020-11-15 19:03:30   139      90         73  6.67    37.7 
#>  8 2020-11-15 19:03:31   127      92         77  6.67    44.3 
#>  9 2020-11-15 19:03:32   140      91         79  6.67    51   
#> 10 2020-11-15 19:03:33   158      91         81  6.67    57.7 
#> # ℹ 3,591 more rows
```
