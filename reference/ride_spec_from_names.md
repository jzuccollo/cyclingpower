# Build a ride spec from filenames

Assumes files are named `<run><sep><source>`, so `run1_kickr.fit` is the
kickr's recording of run 1. A filename that does not split into exactly
two parts is an error rather than a silent `NA`, which is the failure
mode that previously left every ride labelled with the same missing
source.

## Usage

``` r
ride_spec_from_names(files, sep = "_")
```

## Arguments

- files:

  Character vector of file paths.

- sep:

  Separator between run and source in the filename.

## Value

A tibble with `path`, `run` and `source`, ready for
[`read_rides()`](https://jzuccollo.github.io/cyclingpower/reference/read_rides.md).

## See also

Other reading:
[`read_file()`](https://jzuccollo.github.io/cyclingpower/reference/read_file.md),
[`read_rides()`](https://jzuccollo.github.io/cyclingpower/reference/read_rides.md)

## Examples

``` r
ride_spec_from_names(c("run1_kickr.fit", "run1_4iiii.fit"))
#> # A tibble: 2 × 3
#>   path           run   source
#>   <chr>          <chr> <chr> 
#> 1 run1_kickr.fit run1  kickr 
#> 2 run1_4iiii.fit run1  4iiii 
```
