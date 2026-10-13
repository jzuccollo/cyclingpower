# Put meters from different rides on one scale

Three meters cannot always be ridden together. The usual shape is that
run one carries A and B, run two carries B and C, and you want A against
C even though they never turned a pedal at the same time. B is the
bridge: each run gives a gain relative to it, and the difference between
those two gains is A against C.

## Usage

``` r
chain_sources(
  aligned,
  bridge = NULL,
  min_power = 0,
  block = 60,
  replicates = 1000
)

chain_report(chained)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

- bridge:

  Source common to more than one run. Found automatically when exactly
  one source qualifies.

- min_power:

  Both sources must exceed this to count, in watts.

- block:

  Bootstrap block length in seconds.

- replicates:

  Number of bootstrap replicates.

- chained:

  Result of `chain_sources()`.

## Value

A tibble with one row per source pair, giving the gain of the first
relative to the second as a percentage with its interval, and a `basis`
column recording whether the pair was observed together (`direct`) or
linked through the bridge (`chained`). Diagnostics are attached; see
`chain_report()`.

## Details

The gains are estimated by taking the log ratio second by second within
a run, which cancels the true power without having to model it, and the
interval comes from resampling the two runs independently.

This rests on B reading the same on both days, which the data cannot
check: any drift in B between rides transfers directly onto the A-to-C
comparison and nothing in the ride files distinguishes that from a real
difference. What can be checked is reported by `chain_report()` —
whether the two runs covered comparable power, since gains compared over
different parts of the range are not comparable.

## See also

Other chaining:
[`bridge_candidates()`](https://jzuccollo.github.io/cyclingpower/reference/bridge_candidates.md),
[`chain_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/chain_by_level.md)

## Examples

``` r
# The demo has assioma in both runs, so it bridges kickr to quarq
aligned <- read_rides(demo_rides()) |> align_rides()
chain_sources(aligned)
#> assioma reads 2.6% higher than kickr (95% CI 2.4 to 2.7)
#> assioma reads 0.6% lower than quarq (95% CI -0.4 to 1.7)
#> kickr reads 3.1% lower than quarq (95% CI 2.1 to 4.2), chained via assioma
#> 
#> A chained pair assumes the bridge read identically on both days. 
#> The rides cannot test that. See chain_report().
#> 
#> # A tibble: 3 × 7
#>   source_a source_b basis   gain_pc gain_lo gain_hi via    
#>   <chr>    <chr>    <chr>     <dbl>   <dbl>   <dbl> <chr>  
#> 1 assioma  kickr    direct    2.58     2.43   2.74  NA     
#> 2 assioma  quarq    direct   -0.613   -1.72   0.410 NA     
#> 3 kickr    quarq    chained  -3.11    -4.21  -2.09  assioma
```
