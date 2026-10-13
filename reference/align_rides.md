# Put sources onto a common one-second clock

Two meters recording the same ride do not necessarily agree about what
time it is. When each records to its own head unit the clocks can differ
by several seconds, and differencing the series on nominal timestamps
then compares power from one moment against power from another. That
inflates the spread, shifts the bias, and looks entirely plausible on
the way out.

## Usage

``` r
align_rides(rides, max_offset = 30)

alignment_report(aligned)
```

## Arguments

- rides:

  Long tibble from
  [`read_rides()`](https://jzuccollo.github.io/cyclingpower/reference/read_rides.md).

- max_offset:

  Largest offset to consider, in seconds. Set to `0` to trust the
  recorded timestamps and skip estimation.

- aligned:

  Result of `align_rides()`.

## Value

A tibble of `run`, `source`, `t` (seconds from the start of the run),
`time` and the ride channels, with an alignment report attached.
Retrieve the report with `alignment_report()`.

## Details

Within each run every source is resampled onto a one-second grid, then
the offset against a reference source is estimated by maximising the
cross-correlation of the power series. Where both channels came from a
single head unit the estimate is zero, so the same path serves both
cases. Gaps left by pauses or smart recording stay `NA` rather than
being interpolated across.

## Examples

``` r
aligned <- read_rides(demo_rides()) |> align_rides()

# The demo plants a seven-second offset on one meter, and it comes back
alignment_report(aligned)
#> # A tibble: 4 × 7
#>   run   source  reference offset correlation sharpness status 
#>   <chr> <chr>   <chr>      <dbl>       <dbl>     <dbl> <chr>  
#> 1 run1  assioma assioma        0      NA        NA     assumed
#> 2 run1  kickr   assioma        7       0.996     0.113 aligned
#> 3 run2  assioma assioma        0      NA        NA     assumed
#> 4 run2  quarq   assioma        0       0.997     0.135 aligned
```
