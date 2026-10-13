# The session to ride for a comparison

The block-by-block protocol from
[`vignette("recording_rides")`](https://jzuccollo.github.io/cyclingpower/articles/recording_rides.md),
as data. Each row is one interval: how long, whether the trainer should
hold the target, and what the target is as a fraction of FTP.

## Usage

``` r
comparison_protocol(step_seconds = 120, surges = 4)
```

## Arguments

- step_seconds:

  Seconds per ladder step. The binding constraint is not `min_seconds`
  but how many bootstrap blocks each band carries: the up and down
  ladders land in the same bands, so a band holds roughly
  `2 * step_seconds`, and the default 120 gives it about four 60-second
  blocks. Going shorter drops under three, and resampling two blocks is
  not an estimate. Lengthening to 240 buys about 17% narrower bands for
  twice the ladder.

- surges:

  Number of coast-to-power edges in the sync block.

## Value

A tibble of `block`, `seconds`, `mode`, `target_pc`, `cue` and `why`,
one row per interval, in riding order. `cue` is what the rider is told
at the time; `why` is what the block is for.

## Details

Each block exists for something the pipeline needs, and `why` carries
the reason:

- The **sync surges** are not there to find the offset — when the app
  records, one clock stamps every sample and the offset is zero by
  construction. They are there because
  [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md)
  cannot tell "these clocks agree" from "these traces cannot be compared
  at all" and collapses both into `unresolved`. Without an edge to lock
  onto, a recorder that stamped every meter wrongly passes exactly as a
  correct one does.

- The **ladder** puts a chosen number of seconds into each band, because
  [`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md)
  only has a band if you spent time in it.

- The ladder **runs up and then back down** because a meter reading high
  through warming up gives different numbers at the same target on the
  way down, where a level-dependent meter repeats itself. Ridden one way
  only,
  [`level_dependence()`](https://jzuccollo.github.io/cyclingpower/reference/level_dependence.md)
  cannot separate them.

`mode` is `"erg"` where the trainer holds the target and `"free"` where
it must not: a trainer in erg fights a surge and rounds off the very
edge the sync block exists to provide.

There is one sync block, at the front, and no second one at the end. A
closing block would only detect a clock drifting during the ride, and
when the app records there is one clock stamping every meter — it cannot
drift relative to itself, so the block would cost the best part of two
minutes to rule out something that cannot happen. Independent head units
are a different case, where two clocks can separate; that route is prose
in
[`vignette("recording_rides")`](https://jzuccollo.github.io/cyclingpower/articles/recording_rides.md)
rather than this function, and it should repeat the surges at the end.

## Examples

``` r
comparison_protocol()
#> # A tibble: 21 × 6
#>    block           seconds mode  target_pc cue                             why  
#>    <chr>             <dbl> <chr>     <dbl> <chr>                           <chr>
#>  1 Zero and settle      30 free       0    Zero-offset every meter, then … A sh…
#>  2 Sync surges           8 free       1.5  Out of the saddle, hard         Coas…
#>  3 Sync surges          20 free       0    Stop pedalling completely       Coas…
#>  4 Sync surges           8 free       1.5  Out of the saddle, hard         Coas…
#>  5 Sync surges          20 free       0    Stop pedalling completely       Coas…
#>  6 Sync surges           8 free       1.5  Out of the saddle, hard         Coas…
#>  7 Sync surges          20 free       0    Stop pedalling completely       Coas…
#>  8 Sync surges           8 free       1.5  Out of the saddle, hard         Coas…
#>  9 Sync surges          20 free       0    Stop pedalling completely       Coas…
#> 10 Ladder up           120 erg        0.55 Hold the target and a steady c… Seve…
#> # ℹ 11 more rows
sum(comparison_protocol()$seconds) / 60
#> [1] 23.86667
```
