# The answer, before the table

A comparison returns a tibble of estimates and intervals, and the
question that sent someone here is a sentence: does this meter read high
or low, by how much, and how sure can I be. These methods say that first
and then print the tibble as normal, so nothing downstream changes.

## Usage

``` r
# S3 method for class 'cyclingpower_agreement'
print(x, ...)

# S3 method for class 'cyclingpower_chain'
print(x, ...)
```

## Arguments

- x:

  A result from
  [`power_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/power_agreement.md)
  or
  [`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md).

- ...:

  Passed to the tibble print method.

## Value

`x`, invisibly.

## Examples

``` r
aligned <- read_rides(demo_rides()) |> align_rides()
power_agreement(aligned)
#> run1: assioma reads 2.6% higher than kickr (95% CI 2.4 to 2.7, from 1,800 seconds)
#> run2: assioma reads 0.6% lower than quarq (95% CI -0.4 to 1.7, from 1,800 seconds)
#> 
#> # A tibble: 2 × 13
#>   run   source_a source_b seconds bias_w bias_lo bias_hi sd_difference loa_lo
#>   <chr> <chr>    <chr>      <int>  <dbl>   <dbl>   <dbl>         <dbl>  <dbl>
#> 1 run1  assioma  kickr       1800   4.74    4.23    5.24          5.86  -6.74
#> 2 run2  assioma  quarq       1800  -4.19   -6.81   -1.88         11.0  -25.7 
#> # ℹ 4 more variables: loa_hi <dbl>, gain_pc <dbl>, gain_lo <dbl>, gain_hi <dbl>
chain_sources(aligned)
#> assioma reads 2.6% higher than kickr (95% CI 2.4 to 2.7)
#> assioma reads 0.6% lower than quarq (95% CI -0.5 to 1.7)
#> kickr reads 3.1% lower than quarq (95% CI 2.0 to 4.2), chained via assioma
#> 
#> A chained pair assumes the bridge read identically on both days. 
#> The rides cannot test that. See chain_report().
#> 
#> # A tibble: 3 × 7
#>   source_a source_b basis   gain_pc gain_lo gain_hi via    
#>   <chr>    <chr>    <chr>     <dbl>   <dbl>   <dbl> <chr>  
#> 1 assioma  kickr    direct    2.58     2.42   2.73  NA     
#> 2 assioma  quarq    direct   -0.613   -1.75   0.544 NA     
#> 3 kickr    quarq    chained  -3.11    -4.20  -1.98  assioma
```
