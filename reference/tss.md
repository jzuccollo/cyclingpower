# Training stress score

One hour at threshold scores 100. Duration is taken as the number of
observations, so the data must be sampled once per second; smart
recording will understate it.

## Usage

``` r
tss(power, ftp)
```

## Arguments

- power:

  Numeric vector of per-second power in watts.

- ftp:

  Functional threshold power in watts. No default: a number computed
  against somebody else's threshold looks exactly like a number computed
  against yours.

## Value

Training stress score.

## See also

Other metrics:
[`average_power()`](https://jzuccollo.github.io/cyclingpower/reference/average_power.md),
[`intensity_factor()`](https://jzuccollo.github.io/cyclingpower/reference/intensity_factor.md),
[`normalised_power()`](https://jzuccollo.github.io/cyclingpower/reference/normalised_power.md),
[`variability_index()`](https://jzuccollo.github.io/cyclingpower/reference/variability_index.md)

## Examples

``` r
tss(rep(250, 3600), ftp = 250)
#> [1] 100
```
