# Write a set of demonstration ride files

Builds four CSVs describing two sessions and three meters, with
distortions chosen so every part of the comparison has something to
show. Useful for trying the package, and for
[`launch_app()`](https://jzuccollo.github.io/cyclingpower/reference/launch_app.md)'s
demo button.

## Usage

``` r
demo_rides(dir = file.path(tempdir(), "cyclingpower-demo"))
```

## Arguments

- dir:

  Directory to write into. Created if it does not exist.

## Value

The directory path, invisibly.

## Details

The meters are named after real ones only to make the naming convention
concrete. What they do is planted:

- `assioma` is the reference and appears in both runs, so it is the
  bridge.

- `kickr` reads about 2.5% below it, the difference a drivetrain makes
  between the pedals and a trainer. Its clock also runs 7 seconds ahead,
  so the alignment has an offset to find.

- `quarq` reads low at endurance pace and high above 200 W, the pattern
  a single average difference hides and
  [`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md)
  exists to show.

`kickr` and `quarq` never ride together, so their comparison has to be
chained through `assioma`.

A little noise is added to every meter. Without it a pure gain gives
every second an identical log ratio, the bootstrap distribution is
degenerate, and the intervals collapse to a point — correct, but it
reads as a bug.

## Examples

``` r
dir <- demo_rides(tempfile())
list.files(dir)
#> [1] "run1_assioma.csv" "run1_kickr.csv"   "run2_assioma.csv" "run2_quarq.csv"  
```
