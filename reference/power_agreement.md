# How far apart two meters read

Reports the Bland-Altman summary for every pair of sources in every run:
the mean difference, the spread of that difference, and the limits of
agreement within which most seconds fall. The gain is the same thing on
a multiplicative scale, which is usually the more natural way to read a
power meter's error, since meters tend to be a few per cent out rather
than a fixed number of watts out.

## Usage

``` r
power_agreement(aligned, min_power = 0, block = 60, replicates = 1000)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

- min_power:

  Both sources must exceed this to count, in watts.

- block:

  Bootstrap block length in seconds.

- replicates:

  Number of bootstrap replicates.

## Value

A tibble with one row per source pair per run:

- `run`, `source_a`, `source_b`:

  The pair and the session it came from. `source_a` sorts before
  `source_b` alphabetically, and every difference is `source_a` minus
  `source_b`.

- `seconds`:

  Paired seconds behind the row, after coasting and anything below
  `min_power` is dropped.

- `bias_w`, `bias_lo`, `bias_hi`:

  Mean difference in watts, with its 95% bootstrap interval.

- `sd_difference`:

  Standard deviation of the per-second difference.

- `loa_lo`, `loa_hi`:

  Limits of agreement, `bias_w` +/- 1.96 SD: the range most individual
  seconds fall within. Not an interval on the mean, and much wider than
  one.

- `gain_pc`, `gain_lo`, `gain_hi`:

  The same difference multiplicatively, as a percentage, with its 95%
  bootstrap interval. Usually the more natural reading.

## Details

Intervals come from a moving-block bootstrap, not from treating each
second as independent.

## See also

Other agreement:
[`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md),
[`level_dependence()`](https://jzuccollo.github.io/cyclingpower/reference/level_dependence.md)

## Examples

``` r
aligned <- read_rides(demo_rides()) |> align_rides()
power_agreement(aligned)
#> run1: assioma reads 2.6% higher than kickr (95% CI 2.4 to 2.7, from 1,800 seconds)
#> run2: assioma reads 0.6% lower than quarq (95% CI -0.5 to 1.7, from 1,800 seconds)
#> 
#> # A tibble: 2 × 13
#>   run   source_a source_b seconds bias_w bias_lo bias_hi sd_difference loa_lo
#>   <chr> <chr>    <chr>      <int>  <dbl>   <dbl>   <dbl>         <dbl>  <dbl>
#> 1 run1  assioma  kickr       1800   4.74    4.23    5.18          5.86  -6.74
#> 2 run2  assioma  quarq       1800  -4.19   -6.96   -1.67         11.0  -25.7 
#> # ℹ 4 more variables: loa_hi <dbl>, gain_pc <dbl>, gain_lo <dbl>, gain_hi <dbl>
```
