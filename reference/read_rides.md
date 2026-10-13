# Read several ride files into one long tibble

The unit of analysis is a run (one session, ridden once) recorded by two
or more sources (power meters). This assembles that structure
explicitly, so the `run` and `source` labels come from a spec you can
read.

## Usage

``` r
read_rides(spec)
```

## Arguments

- spec:

  Data frame with columns `path`, `run` and `source`, or the path to a
  directory of `.fit`, `.gpx` or `.csv` files named `<run>_<source>`.

## Value

A tibble on the canonical ride schema with `run` and `source` prepended.

## Details

A directory is accepted as well as a spec, since a folder of files is
what you usually have. The filenames are then read by
[`ride_spec_from_names()`](https://jzuccollo.github.io/cyclingpower/reference/ride_spec_from_names.md),
which needs them to be `<run>_<source>`; build the spec yourself where
they are not.

## See also

Other reading:
[`read_file()`](https://jzuccollo.github.io/cyclingpower/reference/read_file.md),
[`ride_spec_from_names()`](https://jzuccollo.github.io/cyclingpower/reference/ride_spec_from_names.md)

## Examples

``` r
rides <- read_rides(demo_rides())
head(rides)
#> # A tibble: 6 × 4
#>   run   source  time                power
#>   <chr> <chr>   <dttm>              <dbl>
#> 1 run1  assioma 2026-03-14 09:00:00  234.
#> 2 run1  assioma 2026-03-14 09:00:01  227.
#> 3 run1  assioma 2026-03-14 09:00:02  243.
#> 4 run1  assioma 2026-03-14 09:00:03  220.
#> 5 run1  assioma 2026-03-14 09:00:04  248.
#> 6 run1  assioma 2026-03-14 09:00:05  251.
```
