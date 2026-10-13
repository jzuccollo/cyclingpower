# Whether a pair's error changes across the power range

The finding a single average difference hides, stated as a test rather
than left to the eye: the extreme bands' gains, and whether their
intervals are separated.

## Usage

``` r
level_dependence(by_level)
```

## Arguments

- by_level:

  Result of
  [`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md).

## Value

A tibble with one row per source pair per run, giving the lowest and
highest gain, the bands they came from, and `separated`.

## Details

The test is on the per-cent gain, not on watts. A meter that is a
constant 2.5% out reads further from its partner at 300 W than at 100 W,
so a watts comparison of the extreme bands separates for every clean
percentage meter. In per cent, that meter is flat and only a
level-dependent one separates.

Separation is non-overlapping 95% intervals, so there is no threshold
constant to argue about; a pair with too few seconds to resolve anything
does not fire.

## See also

Other agreement:
[`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md),
[`power_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/power_agreement.md)

## Examples

``` r
aligned <- read_rides(demo_rides()) |> align_rides()
agreement_by_level(aligned) |> level_dependence()
#> # A tibble: 2 × 8
#>   run   source_a source_b low_band  low_gain high_band high_gain separated
#>   <chr> <chr>    <chr>    <chr>        <dbl> <chr>         <dbl> <lgl>    
#> 1 run1  assioma  kickr    175–200 W     2.30 75–100 W       3.20 FALSE    
#> 2 run2  assioma  quarq    300–325 W    -5.25 50–75 W        3.71 TRUE     
```
