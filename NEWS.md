# cyclingpower 0.4.0

The release that makes the package installable by someone who did not write it.

## Breaking changes

* The licence is now MIT, was GPL-3.

* `skim_power_data()` is removed. Nothing in the package used it, it had no
  tests, and it was the only reason `skimr` was a dependency. The Coggan
  metrics are exported and compose with `dplyr::summarise()`:
  `summarise(rides, np = normalised_power(power), .by = source)`.

* `drop_zeros()` is removed. It was `power[power != 0]` behind an export, and
  nothing in the package, the app, the report template or the vignettes called
  it. Where you want a metric over pedalling seconds only, subset first:
  `average_power(x[x != 0])`.

* `intensity_factor()` and `tss()` require `ftp` again; the `250` default
  introduced during this cycle is gone. A score computed against somebody
  else's threshold is indistinguishable from one computed against yours, which
  is the same objection that removed `skim_power_data()`.

* `start_recording()`, `record_samples()`, `stop_recording()` and
  `render_comparison()` are no longer exported. They were public only so that
  the Shiny app, which lived in `inst/`, could reach them; it now lives in `R/`
  and does not need them to be.

* `power_agreement()` returns `loa_lo` and `loa_hi`, was `loa_lower` and
  `loa_upper`, to match `bias_lo`/`bias_hi` in the same table.

* `read_file()`'s first argument is `path`, was `file_url`. It takes a path.

* The Coggan metrics take `power`, was `power_data`. They take a numeric
  vector, and the old name read like a data frame.

* `vignette("compare_pms")` is now `vignette("cyclingpower")`, and is built on
  `demo_rides()` rather than on a FIT file, so it renders for everyone rather
  than only for those who have installed FITfileR.

## New

* `power_agreement()` and `chain_sources()` now print the finding as a sentence
  above the table — which meter reads high, by how much, and from how many
  seconds. A chained comparison prints the assumption it rests on without being
  asked for it.

* `read_rides()` and `compare_report()` take a directory as well as a spec, so
  a folder of ride files needs no setup:
  `read_rides("rides/") |> align_rides() |> power_agreement()`.

* `tss()`, `intensity_factor()`, `collapse_protocol()` and `write_workout()`
  default to `ftp = 250`, which is what the app, the protocol and the recording
  vignette already assumed. Set your own.

* Every exported function now has a runnable example, and the reference index
  is grouped by what the functions do.

## Under the hood

* The Shiny app moved from `inst/shiny/app.R` into `R/`, split into
  `app-ui.R`, `app-server.R` and `app-theme.R`. It is now measured by covr and
  checked by `R CMD check` like the rest of the package, and its analysis path
  has tests that drive the real reactive graph through `shiny::testServer()`.
  The recorder's radio path remains untested and untestable; see
  `tests/testthat/test-record.R`.

* `rmarkdown` moved to `Suggests`. With `skimr` gone, the `Imports` tree falls
  from 55 packages to 32.

* The directory scanner behind the app's Rides tab is the one `read_rides()`
  uses, so the two cannot disagree about what counts as a ride file.

* `DESCRIPTION` declares `R (>= 4.1)`, which the package has needed since it
  started using the native pipe.
