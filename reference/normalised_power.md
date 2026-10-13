# Normalised power

The fourth-power mean of a 30-second rolling average, which weights hard
efforts more heavily than a plain average does and so tracks the
physiological cost of a ragged ride better than average power.

## Usage

``` r
normalised_power(power)
```

## Arguments

- power:

  Numeric vector of per-second power in watts.

## Value

Normalised power in watts.

## Details

Follows Coggan's definition: the rolling mean runs over the whole
recording with coasting included and counted as zero, and trails rather
than centres, so the first value falls at the thirty-second mark. A
recording shorter than the window has no normalised power and returns
`NA`. For a value over the seconds actually spent pedalling, drop the
coasting yourself first: `normalised_power(x[x != 0])`.

## See also

Other metrics:
[`average_power()`](https://jzuccollo.github.io/cyclingpower/reference/average_power.md),
[`intensity_factor()`](https://jzuccollo.github.io/cyclingpower/reference/intensity_factor.md),
[`tss()`](https://jzuccollo.github.io/cyclingpower/reference/tss.md),
[`variability_index()`](https://jzuccollo.github.io/cyclingpower/reference/variability_index.md)

## Examples

``` r
normalised_power(rep(250, 600))
#> [1] 250
```
