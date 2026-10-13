# Power over time, by source

The first look at any comparison: whether the traces sit on top of each
other, and whether they are in step. A rolling mean is drawn over the
second-by-second trace, which is too noisy to read directly.

## Usage

``` r
plot_timeseries(aligned, smooth = 30)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

- smooth:

  Window of the rolling mean, in seconds.

## Value

A ggplot object.

## See also

Other plots:
[`plot_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/plot_agreement.md),
[`plot_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/plot_by_level.md),
[`plot_chain()`](https://jzuccollo.github.io/cyclingpower/reference/plot_chain.md)

## Examples

``` r
read_rides(demo_rides()) |> align_rides() |> plot_timeseries()
#> Warning: Removed 14 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 130 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
