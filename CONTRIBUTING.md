# Contributing to cyclingpower

Issues and pull requests are welcome. This is a one-maintainer package,
so please open an issue before starting anything large — it may already
be a deliberate omission.

## Before you start

``` r

devtools::install_dev_deps()
devtools::load_all()
devtools::test()
devtools::check()
```

`FITfileR` is not on CRAN and is fetched from the `Remotes:` entry.
Without it the FIT tests skip rather than fail; everything else still
runs.

## How this package is tested

This is the part worth reading before you write a test.

A characterisation test on a real ride file shows only that the code
ran. The suite instead **plants a distortion it chose and asserts the
machinery recovers it** — see `tests/testthat/helper-synthetic.R` for
`make_source()` (gain, offset, lag, noise), `make_level_dependent()` and
`featureless_rides()`, and
[`demo_rides()`](https://jzuccollo.github.io/cyclingpower/reference/demo_rides.md)
for a whole set of files with known answers in them. When you add a
capability, plant a known answer for it. Do not pin whatever the code
currently emits.

Two traps that will bite you:

- Anything touching the bootstrap must be wrapped in
  [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  or it is flaky.
- A *pure* gain with no noise gives every second an identical log ratio,
  so the bootstrap distribution is degenerate and the interval collapses
  to a point. That is correct. Add noise when testing that an interval
  brackets a value.

## The recorder

`R/record.R` and `inst/shiny/www/record.js` read power meters over Web
Bluetooth. The packet parser is tested under V8 and the file writing is
tested normally. **The radio path — `requestDevice`, the GATT
connection, notifications, the reconnection loop, the wake lock — is not
tested and cannot be.** It needs a physical meter, a paired browser and
a user gesture.

Please do not add a mock for it. A mock here would test the mock, not
the parser the browser actually runs, and the credibility of the rest of
the suite rests on the untested part being visibly marked rather than
papered over.

Several things in that file look like they could be simplified and
cannot:

- Samples are batched at 500 ms because Shiny collapses repeat
  `setInputValue` calls of the same name within a tick, which silently
  loses samples from a 4 Hz meter.
- A gap is never padded with zeros. Zeros read as coasting, coasting is
  dropped as a second both meters agreed on, and a dropped connection
  would then enter the comparison as evidence of agreement.
- Stopping is ordered: a capture-phase click listener queues the final
  batch before Shiny sends the button press, and the `input$samples`
  observer has `priority = 10` against the stop observer’s `-10`.
- `record.js` addresses `#pair_meter` and `#stop_record` directly, so
  the app’s Record tab must not be wrapped in a Shiny module namespace.

**Do not add trainer control.** It was considered and rejected; the
reasoning is in `CLAUDE.md`. The package emits a workout file and
something else rides it.

## Code style

- Native pipe `|>`, `<-` for assignment, `snake_case`.
- Explicit `pkg::` prefixes in `R/`. Shiny is in `Suggests`, so the app
  code qualifies every call rather than attaching the package.
- `.groups = "drop"` in every `summarise()`.
- Comments explain *why*, not *what*.
- Minimal defensive coding. Errors are for genuine boundaries — an
  unreadable file, an impossible chain — not for guarding internal
  states.
- `NAMESPACE` and `man/` are roxygen-generated. Run
  `devtools::document()`; never hand-edit them.

## Pull requests

Fork, branch, and make sure `devtools::check()` is clean before you open
it. Add a bullet to `NEWS.md` for anything a user would notice.
