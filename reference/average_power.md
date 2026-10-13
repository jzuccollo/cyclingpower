# Average power

The mean over the whole recording, counting coasting as zero, which is
what variability index and training stress score are defined against.
For the average of the seconds actually spent pedalling, drop the
coasting yourself first: `average_power(x[x != 0])`.

## Usage

``` r
average_power(power)
```

## Arguments

- power:

  Numeric vector of per-second power in watts.

## Value

Average power in watts.

## See also

Other metrics:
[`intensity_factor()`](https://jzuccollo.github.io/cyclingpower/reference/intensity_factor.md),
[`normalised_power()`](https://jzuccollo.github.io/cyclingpower/reference/normalised_power.md),
[`tss()`](https://jzuccollo.github.io/cyclingpower/reference/tss.md),
[`variability_index()`](https://jzuccollo.github.io/cyclingpower/reference/variability_index.md)

## Examples

``` r
average_power(c(rep(200, 1800), rep(0, 1800)))
#> [1] 100
```
