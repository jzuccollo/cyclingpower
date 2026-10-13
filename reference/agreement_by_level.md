# How the difference between two meters varies with power

Meters often agree at endurance pace and diverge at threshold or in
sprints, which a single average difference hides entirely. Seconds are
binned by the mean of the two readings rather than by either one of
them: binning on one meter and then plotting its own difference against
that bin makes measurement noise look like a real trend.

## Usage

``` r
agreement_by_level(
  aligned,
  width = 25,
  min_seconds = 30,
  min_power = 0,
  block = 60,
  replicates = 500
)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

- width:

  Bin width in watts.

- min_seconds:

  Bins with fewer paired seconds than this are dropped.

- min_power:

  Both sources must exceed this to count, in watts.

- block:

  Bootstrap block length in seconds.

- replicates:

  Number of bootstrap replicates. Lower than elsewhere in the package
  because every band is resampled separately, so 500 across eight bands
  is already more work than 1000 over the ride as a whole.

## Value

A tibble with one row per bin per source pair per run:

- `run`, `source_a`, `source_b`:

  As
  [`power_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/power_agreement.md).

- `band`, `band_range`:

  Bin centre in watts, and its label. Seconds are binned on the mean of
  the two meters, never on either one.

- `seconds`, `mean_power`:

  Paired seconds in the bin, and their mean level.

- `bias_w`, `bias_lo`, `bias_hi`:

  Mean difference in watts within the bin, with its 95% bootstrap
  interval.

- `gain_pc`, `gain_lo`, `gain_hi`:

  The same as a percentage.

Seconds dropped for thin bins are carried on the `dropped_seconds`
attribute.

## Details

Time is not spread evenly across the power range, so `seconds` is
carried through on every row. Bins thinner than `min_seconds` are
dropped, and how many seconds that discarded is reported as an
attribute.

## See also

Other agreement:
[`level_dependence()`](https://jzuccollo.github.io/cyclingpower/reference/level_dependence.md),
[`power_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/power_agreement.md)

## Examples

``` r
aligned <- read_rides(demo_rides()) |> align_rides()
agreement_by_level(aligned)
#> # A tibble: 23 × 13
#>    run   source_a source_b  band band_range seconds mean_power bias_w bias_lo
#>    <chr> <chr>    <chr>    <dbl> <chr>        <int>      <dbl>  <dbl>   <dbl>
#>  1 run1  assioma  kickr     87.5 75–100 W        31       94.5   2.85   1.45 
#>  2 run1  assioma  kickr    112.  100–125 W      152      115.    3.37   2.03 
#>  3 run1  assioma  kickr    138.  125–150 W      309      138.    3.66   2.85 
#>  4 run1  assioma  kickr    162.  150–175 W      355      162.    3.77   2.95 
#>  5 run1  assioma  kickr    188.  175–200 W      338      187.    4.24   3.93 
#>  6 run1  assioma  kickr    212.  200–225 W      265      211.    5.23   4.41 
#>  7 run1  assioma  kickr    238.  225–250 W      148      237.    6.69   5.92 
#>  8 run1  assioma  kickr    262.  250–275 W       77      261.    7.29   5.96 
#>  9 run1  assioma  kickr    288.  275–300 W       45      285.    7.78   6.78 
#> 10 run2  assioma  quarq     62.5 50–75 W         34       66.1   2.57  -0.778
#> # ℹ 13 more rows
#> # ℹ 4 more variables: bias_hi <dbl>, gain_pc <dbl>, gain_lo <dbl>,
#> #   gain_hi <dbl>
```
