# Difference between two meters across the power range

The summary a single average difference hides. Each band shows the mean
difference over the seconds spent at that power, with its confidence
interval; the bars beneath show how many seconds sit behind each
estimate, because time is not spread evenly across the range and the
ends are usually thin.

## Usage

``` r
plot_by_level(by_level, scale = c("watts", "percent"))
```

## Arguments

- by_level:

  Result of
  [`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md).

- scale:

  `"watts"` for the difference in watts, `"percent"` for the gain. A
  constant percentage error is a rising line in watts and a flat line in
  per cent, so the two say different things about the same meter.

## Value

A ggplot object.

## Details

On the per-cent scale the caption says why the two scales differ: a
meter that looks level-dependent in watts is often only a constant
percentage out.

## See also

Other plots:
[`plot_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/plot_agreement.md),
[`plot_chain()`](https://jzuccollo.github.io/cyclingpower/reference/plot_chain.md),
[`plot_timeseries()`](https://jzuccollo.github.io/cyclingpower/reference/plot_timeseries.md)

## Examples

``` r
read_rides(demo_rides()) |>
  align_rides() |>
  agreement_by_level() |>
  plot_by_level()
```
