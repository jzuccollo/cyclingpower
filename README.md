
# cyclingpower

<!-- badges: start -->
<!-- badges: end -->

Compare the readings from two or three power meters that recorded the same
riding, and quantify how far apart they are — overall, and across the power
range, since meters often agree at endurance pace and diverge at threshold.

Still a personal project built around a specific problem, but the awkward parts
are handled properly: files are put on a common clock before anything is
differenced, intervals account for the fact that per-second power is heavily
autocorrelated, and a third meter can be brought onto the same scale as the
first two even though it never rode alongside them.

## Installation

``` r
remotes::install_github("jzuccollo/cyclingpower")
```

Reading FIT files needs [FITfileR](https://github.com/grimbough/FITfileR),
which is not on CRAN. It is declared in `Remotes:`, so the command above will
fetch it, but it can be installed directly:

``` r
remotes::install_github("grimbough/FITfileR")
```

GPX and CSV files need nothing extra. Note that exports only carry power a
meter actually recorded — power that Strava estimated from speed and gradient
is not included, so a ride done without a meter reads as having no power at all.

The interactive app additionally needs a few packages:

``` r
install.packages(c("shiny", "bslib", "DT", "shinyFiles"))
```

## Recording the rides

The comparison is only as good as the recording, and the ways it goes wrong
cannot be repaired afterwards. A power meter broadcasts but a recorder records
one power source, so two meters need two recorders — ANT+ is one-to-many, so
pairing each meter to its own head unit over ANT+ avoids the Bluetooth
one-connection limit.

Then, on every recorder:

- Record **every second**, not "smart" recording.
- **Auto-pause off.** A pause on one unit and not the other breaks the single
  clock offset that alignment assumes.
- Let each unit take its clock from GPS before starting.
- Zero every meter after a warm-up, immediately before the run.

And ride the session in two parts. Open with four to six short surges from
coasting, which is what `align_rides()` cross-correlates to recover the clock
offset — a clean ERG block is the most controlled ride you can do and the worst
one for establishing what the offset was. Then hold five or six steady steps
across the power range, so `agreement_by_level()` has enough seconds in each
bin to say anything.

`vignette("recording_rides")` has the full protocol, including chaining a third
meter across two sessions and what to expect when comparing a trainer against a
pedal or crank meter.

## Usage

Name your files `<run>_<source>`, so `run1_kickr.fit` is the Kickr's recording
of run one, and the whole comparison is three calls:

``` r
library(cyclingpower)

aligned <- list.files("rides", full.names = TRUE) |>
  ride_spec_from_names() |>
  read_rides() |>
  align_rides()

power_agreement(aligned)      # overall difference, in watts and per cent
agreement_by_level(aligned)   # the same, across the power range
chain_sources(aligned)        # meters linked through a common bridge meter
```

## Doing it interactively

`launch_app()` opens a local web interface over the same pipeline: browse to a
directory, correct the run and source labels if the filenames do not carry
them, and read the results with the settings live. Each screen explains how to
read what it is showing.

``` r
launch_app()
```

**Load demo rides** builds a worked example, so there is something to look at
before you have files of your own. Three meters across two sessions, with the
errors planted: `assioma` rides both sessions and is the bridge, `kickr` reads
about 2.5 per cent below it with a seven-second clock offset, and `quarq` reads
low at endurance pace and high above 200 W.

That last one is the case worth seeing. `quarq`'s overall bias is about −0.6
per cent, which reads as near-perfect agreement, and the power-range breakdown
shows it swinging from +3 per cent to −5 per cent. The same files are available
outside the app from `demo_rides()`.

The app earns its place mainly for two things: the alignment report is the
first screen rather than something you have to remember to inspect, and the
bands in `agreement_by_level()` are much easier to judge when you can move the
bin width and watch the seconds per band move with it.

The report is still what you keep. The app's download button renders it from
the results already on screen, so the file cannot disagree with what you were
looking at when you saved it.

## Doing it in one call

`compare_report()` runs all of it and writes an HTML document:

``` r
list.files("rides", full.names = TRUE) |>
  ride_spec_from_names() |>
  compare_report("power-comparison.html")
```

## Why alignment comes first

When each meter records to its own head unit the clocks can differ by several
seconds. Differencing the series on nominal timestamps then compares power from
one moment against power from another, which inflates the spread and shifts the
average difference — and the result looks entirely plausible on the way out. On
a test ride with a true three per cent gain and a seven-second offset,
differencing on raw timestamps reports 6.2 per cent.

`align_rides()` recovers the offset by cross-correlating the power traces, and
reports what it found. Where both channels came off one head unit the offset is
zero and nothing changes. Where a ride was too steady to lock onto, it says so
and falls back to the recorded timestamps rather than guessing.

## Chaining a third meter

Run one carries meters A and B, run two carries B and C, and you want A against
C. B is the bridge: each run gives a gain relative to it, and the difference
between those gains is A against C. `chain_sources()` marks such comparisons as
`chained` rather than `direct`, because they rest on B having read the same on
both days — an assumption the ride files cannot test. `chain_report()` states
it, and reports whether the two runs at least covered comparable power.

## Code of Conduct

Please note that the cyclingpower project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
