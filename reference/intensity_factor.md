# Intensity factor

Normalised power as a fraction of threshold power.

## Usage

``` r
intensity_factor(power, ftp)
```

## Arguments

- power:

  Numeric vector of per-second power in watts.

- ftp:

  Functional threshold power in watts. No default: a number computed
  against somebody else's threshold looks exactly like a number computed
  against yours.

## Value

Intensity factor, where 1 is riding at threshold.

## See also

Other metrics:
[`average_power()`](https://jzuccollo.github.io/cyclingpower/reference/average_power.md),
[`normalised_power()`](https://jzuccollo.github.io/cyclingpower/reference/normalised_power.md),
[`tss()`](https://jzuccollo.github.io/cyclingpower/reference/tss.md),
[`variability_index()`](https://jzuccollo.github.io/cyclingpower/reference/variability_index.md)

## Examples

``` r
intensity_factor(rep(210, 600), ftp = 250)
#> [1] 0.84
```
