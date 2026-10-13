# Gains between meters on one scale

Shows every pairwise comparison as a percentage, separating the pairs
that were ridden together from those linked through a bridge meter. The
chained comparisons carry the uncertainty of both rides and rest on the
bridge having read the same on both days, so they are drawn distinctly
rather than presented as equivalent evidence.

## Usage

``` r
plot_chain(chained)
```

## Arguments

- chained:

  Result of
  [`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md).

## Value

A ggplot object.

## See also

Other plots:
[`plot_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/plot_agreement.md),
[`plot_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/plot_by_level.md),
[`plot_timeseries()`](https://jzuccollo.github.io/cyclingpower/reference/plot_timeseries.md)

## Examples

``` r
read_rides(demo_rides()) |> align_rides() |> chain_sources() |> plot_chain()
```
