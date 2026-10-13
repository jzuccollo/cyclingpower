# How the chained comparison varies with power

The level-wise counterpart to
[`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md),
and on the same rule: a pair that rode together is compared within each
power band from its own seconds, and only a pair that never did is
chained through the bridge. The bands kept are those every run actually
covered, since a gain chained from bands one run never rode is
extrapolation rather than measurement.

## Usage

``` r
chain_by_level(
  aligned,
  bridge = NULL,
  width = 25,
  min_seconds = 30,
  min_power = 0
)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

- bridge:

  Source common to more than one run. Found automatically when exactly
  one source qualifies.

- width:

  Bin width in watts.

- min_seconds:

  Bins with fewer paired seconds than this are dropped.

- min_power:

  Both sources must exceed this to count, in watts.

## Value

A tibble with one row per band per source pair, restricted to bands
covered by every run involved.

## See also

Other chaining:
[`bridge_candidates()`](https://jzuccollo.github.io/cyclingpower/reference/bridge_candidates.md),
[`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md)

## Examples

``` r
read_rides(demo_rides()) |> align_rides() |> chain_by_level()
#> # A tibble: 27 × 4
#>    source_a source_b  band gain_pc
#>    <chr>    <chr>    <dbl>   <dbl>
#>  1 assioma  kickr     87.5    3.20
#>  2 assioma  kickr    112.     2.98
#>  3 assioma  kickr    138.     2.70
#>  4 assioma  kickr    162.     2.37
#>  5 assioma  kickr    188.     2.30
#>  6 assioma  kickr    212.     2.52
#>  7 assioma  kickr    238.     2.86
#>  8 assioma  kickr    262.     2.84
#>  9 assioma  kickr    288.     2.77
#> 10 assioma  quarq     87.5    2.98
#> # ℹ 17 more rows
```
