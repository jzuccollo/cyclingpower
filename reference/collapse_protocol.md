# The protocol as blocks to read

[`comparison_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/comparison_protocol.md)
is one row per interval, which a workout file needs and a rider does
not. This collapses consecutive rows back into the blocks they came
from, so the table on the Record tab and the one in
[`vignette("recording_rides")`](https://jzuccollo.github.io/cyclingpower/articles/recording_rides.md)
are both the protocol rather than a transcription of it that can drift
from the workout file.

## Usage

``` r
collapse_protocol(protocol, ftp = 250)
```

## Arguments

- protocol:

  One row per interval, as
  [`comparison_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/comparison_protocol.md)
  returns.

- ftp:

  Functional threshold power in watts, which turns the fractional
  targets into the watts the trainer will be asked to hold. The default
  is the package's reference rider; set your own.

## Value

A data frame of `Block`, `Duration`, `Mode`, `Target` and `Why`, one row
per block, with a closing `Total`.

## Examples

``` r
collapse_protocol(comparison_protocol(), ftp = 250)
#>              Block         Duration           Mode
#> 1  Zero and settle             30 s              —
#> 2      Sync surges 4 × (8 s / 20 s) Level or slope
#> 3        Ladder up        5 × 2 min            Erg
#> 4            Coast            1 min              —
#> 5      Ladder down        5 × 2 min            Erg
#> 6    Stop together             30 s              —
#> 11           Total           24 min               
#>                                   Target
#> 1                               Coasting
#> 2          About 375 W, coasting between
#> 3  138 W → 175 W → 212 W → 250 W → 275 W
#> 4                               Coasting
#> 5  275 W → 250 W → 212 W → 175 W → 138 W
#> 6                               Coasting
#> 11                                      
#>                                                                                                         Why
#> 1                                                    A shared floor of true zeros at the head of every file
#> 2                                Coast-to-power edges, so check_alignment() can resolve rather than default
#> 3                                                               Several hundred stable seconds in each band
#> 4                                                                               Exercises the coasting drop
#> 5                                                                         The same bands, later in the ride
#> 6                                                                                             A shared tail
#> 11 Ride it the same way every time. Chained comparisons assume the bridge meter reads the same on both days
```
