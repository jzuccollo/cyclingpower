# Variability index

Normalised power over average power: how ragged the effort was. A steady
time trial sits near 1, a criterium or a hilly group ride well above it.

## Usage

``` r
variability_index(power)
```

## Arguments

- power:

  Numeric vector of per-second power in watts.

## Value

Variability index, where 1 is a perfectly steady effort.

## See also

Other metrics:
[`average_power()`](https://jzuccollo.github.io/cyclingpower/reference/average_power.md),
[`intensity_factor()`](https://jzuccollo.github.io/cyclingpower/reference/intensity_factor.md),
[`normalised_power()`](https://jzuccollo.github.io/cyclingpower/reference/normalised_power.md),
[`tss()`](https://jzuccollo.github.io/cyclingpower/reference/tss.md)

## Examples

``` r
variability_index(rep(250, 600))
#> [1] 1
```
