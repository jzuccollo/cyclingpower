# Render the whole comparison from a set of files

The one-call entry point: hand it a spec of ride files and it reads
them, puts them on a common clock, and reports how the meters compare
overall, across the power range, and — where one meter is common to more
than one run — between meters that never rode together.

## Usage

``` r
compare_report(
  spec,
  output_file = "power-comparison.html",
  min_power = 0,
  width = 25,
  min_seconds = 30,
  block = 60,
  replicates = 1000,
  max_offset = 30,
  smooth = 30,
  seed = NULL
)
```

## Arguments

- spec:

  Data frame with columns `path`, `run` and `source`, as built by
  [`ride_spec_from_names()`](https://jzuccollo.github.io/cyclingpower/reference/ride_spec_from_names.md),
  or the path to a directory of ride files named `<run>_<source>`.

- output_file:

  Path of the HTML file to write.

- min_power:

  Both sources must exceed this to count, in watts.

- width:

  Bin width for the power-range breakdown, in watts.

- min_seconds:

  Power bands with fewer paired seconds than this are dropped.

- block:

  Bootstrap block length in seconds.

- replicates:

  Number of bootstrap replicates.

- max_offset:

  Largest clock offset to consider, in seconds.

- smooth:

  Window of the rolling mean on the power traces, in seconds.

- seed:

  Seed for the bootstrap. The intervals are resampled, so two runs over
  the same rides differ slightly unless this is set.

## Value

The path of the rendered file, invisibly.

## Examples

``` r
compare_report(demo_rides(), tempfile(fileext = ".html"))
```
