# Sources that could bridge two runs

A meter can only link runs it appeared in more than one of. This is what
[`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md)
searches when `bridge` is left unset, exposed separately so anything
offering the choice to a user works from the same list.

## Usage

``` r
bridge_candidates(aligned)
```

## Arguments

- aligned:

  Result of
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md).

## Value

A character vector of source names, empty when nothing qualifies.

## See also

Other chaining:
[`chain_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/chain_by_level.md),
[`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md)

## Examples

``` r
read_rides(demo_rides()) |> align_rides() |> bridge_candidates()
#> [1] "assioma"
```
