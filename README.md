
# cyclingpower

<!-- badges: start -->
[![R-CMD-check](https://github.com/jzuccollo/cyclingpower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jzuccollo/cyclingpower/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jzuccollo/cyclingpower/graph/badge.svg)](https://app.codecov.io/gh/jzuccollo/cyclingpower)
<!-- badges: end -->

Compare the readings of two or three power meters that recorded the same
riding. Reads FIT, GPX and CSV files, puts them on a common clock, and reports
how far apart the meters are overall and across the power range. A third meter
can be linked in through a bridge meter
common to both sessions.

## Installation

``` r
remotes::install_github("jzuccollo/cyclingpower")
```

FIT files need [FITfileR](https://github.com/grimbough/FITfileR), which is not
on CRAN. It is declared in `Remotes:` so the command above fetches it. GPX and
CSV need nothing extra. Install dependencies with:

``` r
install.packages(c("shiny", "bslib", "DT", "shinyFiles"))
```

## Usage

Name your files `<run>_<source>`, so `run1_kickr.fit` is the Kickr's recording
of run one, and pass `read_rides()` the directory.

``` r
library(cyclingpower)

aligned <- read_rides("rides") |> align_rides()

power_agreement(aligned)      # overall difference, in watts and per cent
# run1: assioma reads 2.6% higher than kickr (95% CI 2.4 to 2.7, from 1,800 seconds)

agreement_by_level(aligned)   # the same, across the power range
chain_sources(aligned)        # meters linked through a common bridge meter
```

Where the filenames do not carry the labels, build the table yourself with
`ride_spec_from_names()` or by hand and pass that instead. `compare_report()`
runs the whole pipeline and writes an HTML report:

``` r
compare_report("rides", "power-comparison.html")
```

To see an example, run
`demo_rides()` and follow `vignette("cyclingpower")`.

## The app

`launch_app()` opens a local web interface with the same pipeline: pick a
directory, fix the run and source labels if the filenames don't carry them, and
adjust settings. **Load demo rides** builds a worked example with known
errors planted, also available outside the app from `demo_rides()`.

The app can also record the meters itself over Web Bluetooth (Chrome or Edge,
Record tab). The radio has no automated tests, so check the first session
against a head unit you trust.

## Recording

If each meter records to its own head unit, then on every unit: record every
second (not "smart" recording), turn auto-pause off, let the clock set from GPS,
and zero the meter after a warm-up. Ride four to six short surges from coasting
so `align_rides()` has features to cross-correlate, then five or six steady
steps across the power range so `agreement_by_level()` has time in each bin.

Recording in the app fixes the alignment problem, so you only need an erg ladder up and back down, with enough time at each level. However, it's worth keeping a short block of surges anyway, just in case.

`vignette("recording_rides")` has a ride protocol and can export a .zwo for you to ride in erg.

## Why alignment matters

Independent head units can differ by several seconds. Differencing on nominal
timestamps then compares one moment's power against another's, which inflates
the spread and shifts the mean difference.

`align_rides()` recovers the offset by cross-correlating the power traces. The main challenge is where a turbo ride is too steady to lock onto it. Then it will say so and
fall back to the recorded timestamps.

## Chaining a third meter (bridging)

Run one carries meters A and B, run two carries B and C, and you want A against
C. B is the bridge and each run gives a gain relative to it, so the difference
between those gains is A against C. `chain_sources()` marks these as `chained`
rather than `direct`, because they assume B read the same on both days. `chain_report()` calculates and reports
whether the two runs covered comparable power.

## Licence

MIT © James Zuccollo. Contributions welcome; see
[CONTRIBUTING.md](.github/CONTRIBUTING.md), which explains how the package is
tested and what in the recorder must not be simplified.
