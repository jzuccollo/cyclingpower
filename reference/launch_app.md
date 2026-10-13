# Explore a comparison interactively

Opens a local web interface over the same pipeline as
[`compare_report()`](https://jzuccollo.github.io/cyclingpower/reference/compare_report.md):
point it at a directory of ride files, correct the run and source labels
if the filenames do not carry them, and read the results with the
settings live.

## Usage

``` r
launch_app(dir = NULL, ...)
```

## Arguments

- dir:

  Directory of ride files to open with. Can also be set in the app.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), for
  `port` and `host`.

## Value

Nothing useful; called for the running app.

## Details

The report is the thing to keep, being a file you can send or come back
to. The app's download button renders it from the results already on
screen, so the two cannot disagree.

## Examples

``` r
if (FALSE) { # \dontrun{
launch_app("~/rides")
} # }
```
