# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R package (`cyclingpower`) for comparing the readings of two or three power meters that recorded the same riding. The local directory is `cyclingpoweR` but the package — and therefore `library()`, `::` and `test_check()` — is `cyclingpower`. The GitHub repo matches the package name; only the local folder differs.

## Commands

```r
devtools::load_all()        # load for interactive work
devtools::document()        # regenerate NAMESPACE + man/ after editing roxygen
devtools::test()            # run all tests
testthat::test_file("tests/testthat/test-align.R")   # single file
devtools::check()           # full R CMD check
```

Tests run with the working directory at `tests/testthat/`, but fixtures live in `inst/extdata` and are reached via `system.file("extdata", ..., package = "cyclingpower")` — see `fixture_ride()` in `tests/testthat/helper-synthetic.R`.

`FITfileR` is GitHub-only (`remotes::install_github("grimbough/FITfileR")`) and sits in `Suggests` with a `Remotes:` entry. Anything touching a FIT file needs it, but nothing else does: `fixture_ride()` skips without it, `vignette("cyclingpower")` and every example run off `demo_rides()`, which writes CSVs. Keep it that way — documentation that only renders for people who already installed a second package off GitHub is documentation nobody reads.

## Architecture

A five-layer pipeline. Each layer's output is the next layer's input, and the layers are independently testable.

**Ingest** (`read_file.R`, `read_gpx.R`, `read_csv.R`, `schema.R`, `read_rides.R`) — `read_file()` dispatches on extension and both paths funnel through `canonicalise()`, which renames format-specific aliases and enforces one schema: `time` (POSIXct, UTC) and `power` mandatory, plus optional `cadence`, `heart_rate`, `speed`, `distance`, `altitude`, `temperature`, `lat`, `lon`. **If you add a channel or a reader, add it to `canonical_columns()`/`column_aliases()` rather than special-casing downstream** — the FIT and GPX paths previously drifted into emitting `timestamp` and `time` respectively, which is what the schema layer exists to prevent.

`read_gpx.R` matches on XPath `local-name()` rather than namespace prefixes, so it finds power whether written as Strava's bare `<power>` or Garmin's `gpxpx:PowerInWatts`. A file with no power channel is an error, not an `NA` column.

`read_rides(spec)` takes an explicit `path`/`run`/`source` data frame. The `run` (one session) and `source` (one meter) columns key everything downstream.

**Align** (`align.R`) — the layer the rest depends on being correct. Resamples each source onto a 1 Hz grid relative to the run's earliest sample, averaging duplicate seconds (without which the later `pivot_wider` fans out rows), then estimates each source's clock offset against a within-run reference by cross-correlating power over ±`max_offset`.

Sign convention: `best_lag()` finds `k` maximising `cor(reference[i], candidate[i + k])`, so a positive `k` means the candidate's timestamps run `k` seconds *ahead* and the correction is `t - k`. `test-align.R` pins this in both directions; a sign flip here silently corrupts everything.

`check_alignment()` deliberately collapses "not the same ride" and "steady effort with no features to lock onto" into one `unresolved` status. They cannot be separated by correlation alone, and erroring would block steady trainer sessions, which are the package's main use case. Unresolved means the offset is forced to 0 and a warning fires; the numbers are in `alignment_report()`.

**Compare** (`agreement.R`, `bootstrap.R`) — `paired_seconds()` is the shared workhorse: for each run it pivots wide, forms every source pair via `combn`, and returns one row per pair per second with `difference`, `level` (the pair mean) and `log_ratio`. Coasting seconds are dropped.

Bland-Altman throughout: `level` is the mean of the two meters, never one nominated meter, because binning on one meter and plotting its own difference against that bin turns measurement noise into an apparent slope.

All intervals come from `block_bootstrap_draws()` (moving block, default 60 s). Per-second power is heavily autocorrelated, so an i.i.d. interval is roughly an order of magnitude too narrow. `test-agreement.R` asserts the bootstrap interval is more than 3× the naive one — if that test starts passing trivially, the bootstrap has broken.

**Chain** (`chain.R`) — links meters that never rode together. `resolve_bridge()` finds the source present in more than one run. `pair_estimates()` reduces every pair that actually rode together to one log gain with its draws, and `pool_runs()` combines the runs carrying that pair weighted by seconds — needed as soon as the same pair is ridden twice, and `chain_by_level()` pools the same way band by band so the two views cannot disagree. `bridge_gains()` puts each source on the bridge's scale by negating the pair's estimate where the bridge sorts first alphabetically.

`compare_all_sources()` reports a pair from its own seconds wherever it has any, and only routes through the bridge when it has none — so `basis` says what was actually computed rather than labelling a bridge-routed number `direct`. Chained pairs subtract two independently bootstrapped runs, so their intervals are necessarily wider than the direct ones they are built from. The `basis` column is load-bearing: chained comparisons rest on the bridge reading identically on both days, which the data cannot test. `chain_report()$assumption` says so explicitly — keep that in any output.

**Visualise and report** (`comparison_charts.r`, `report.R`, `inst/rmarkdown/comparison.Rmd`) — plot functions take the thing they visualise: `plot_timeseries()`/`plot_agreement()` take aligned rides, `plot_by_level()` takes `agreement_by_level()` output, `plot_chain()` takes `chain_sources()` output. `compare_report()` renders the parameterised Rmd.

## Recording

The app can also be the recorder, reading BLE power meters over Web Bluetooth (`inst/shiny/www/record.js`, `inst/shiny/www/power-packet.js`, `R/record.R`, `R/tab-record` material inside `R/app-ui.R`/`R/app-server.R`). It sits *before* Ingest and hands over through the filesystem and nothing else: the recorder writes `<run>_<source>.csv`, the Rides tab scans the directory. There is no shared reactive and no data frame passed between the two halves — analysis is repeatable and recording is not, so the boundary keeps a bug in one from costing a ride in the other. Keep it that way.

- **One clock stamps every meter.** The page stamps each sample on arrival with `toISOString()`; nothing the meter reports about time is used and R never re-stamps. That is the whole point — the offset comes out zero *by construction* rather than recovered by cross-correlation. Sub-second stamps must survive to the file, because `resample_1hz()` rounds them onto its grid and averages duplicates, which is how a 4 Hz meter is handled.
- **Never pad a gap with zeros.** `record_samples()` writes a row only when a sample arrives. Zeros would read as coasting, and coasting is dropped as a second both meters agreed on, so a dropped connection would enter the comparison as evidence of agreement. Nothing downstream can tell the two apart.
- **Samples are batched (500 ms) and flushed to disk per batch.** The batching is not an optimisation: Shiny collapses repeat sets of the same input name within a tick, so one `setInputValue` per sample silently loses samples from a 4 Hz meter. The flush is what makes a killed tab cost the last batch rather than the ride.
- **Stopping is ordered.** A capture-phase click listener in `record.js` queues the final batch before Shiny sends the button press, and the `input$samples` observer has `priority = 10` against the stop observer's `-10`. Without both, the last half-second arrives after the files are closed.
- `requestDevice()` needs a user gesture, so the Pair button is handled in JS, never through an observer in R. Custom message handlers must declare exactly one argument — Shiny checks arity and throws, and thrown inside `shiny:connected` that aborts the socket handler so the session renders nothing at all, with no R-side error to find.
- The parser lives in `power-packet.js` with no browser globals in it precisely so `test-record.R` can run it under V8 (Suggests). The radio path has no automated test and cannot have one; `test-record.R` says so at the top and the vignette repeats it. Do not quietly add a mock that looks like coverage.

**The recorder listens and never commands.** Driving the trainer from the page is technically available — FTMS (0x1826) sits alongside Cycling Power on the same connection and its control point takes a target-power write — and it was considered and rejected. There is no browser library to hand it to: the mature options (`incyclist-devices`) are Node-only through `serialport` and native ANT+ bindings, and the one well-shaped browser package (`@deancochran/ftms`) is codecs with no transport, which leaves the half where the bugs live. The training app or head unit already is that library. So the package emits a workout file and something else rides it; **do not add trainer control here** without revisiting that reasoning. Note `record.js` filters on `cycling_power` with no `optionalServices`, so FTMS is not reachable on the connections it makes — control would need a re-pair, not a patch.

**The protocol is data** (`protocol.R`). `comparison_protocol()` returns one row per interval and is the single source for the table on the Record tab, the workout file `write_workout()` emits and the table in `vignette("recording_rides")` — change the session there, not in three places. Every block is justified by a named function's requirement, carried in the `why` column: the sync surges exist because `check_alignment()` collapses "clocks agree" and "not the same ride" into one `unresolved` status, so a *resolved* zero is the only evidence the recorder stamped correctly; the ladder runs up and back down because `level_dependence()` cannot otherwise separate a warming meter from a level-dependent one. The `mode` column is load-bearing: `"free"` blocks must not be ridden in erg, and the `.zwo` writer expresses that as `FreeRide` where the `.erg` format simply cannot.

**What sets the step length is bootstrap blocks per band, not `min_seconds`.** The up and down ladders land in the same bands, so a band carries about `2 * step_seconds`; at the 60 s bootstrap block, the 120 s default leaves roughly four blocks to resample. Measured on synthetic rides with a planted gain over 12 seeds, going 240 s → 120 s halves the session (44 → 24 min) for about 17% wider bands, while 60 s steps give under two blocks per band and stop being an estimate. Do not shorten the ladder further, and do not "fix" a short step by shrinking the bootstrap block — the 60 s block is set by autocorrelation, so shrinking it narrows the interval by assumption rather than by evidence. There is also no closing sync block: it would only detect a clock drifting mid-ride, and one clock stamping every meter cannot drift against itself.

## Conventions

- Follow `~/.claude/rules/r-code-style.md`: native pipe, `<-`, snake_case, explicit `pkg::` prefixes in `R/`, `.groups = "drop"` in every `summarise()`, and minimal defensive coding. Errors are for genuine boundaries — an unreadable file, an impossible chain — not for guarding internal states.
- Charts use `theme_minimal()` and the Okabe-Ito palette in `source_palette()`. Deliberately **not** `epitheme::theme_epi()`, despite the global data-viz rule: epitheme is not on CRAN and this package documents a public `install_github` route.
- `NAMESPACE` and `man/` are roxygen-generated; never hand-edit.
- `.data` comes from `@importFrom rlang .data` in `cyclingpower-package.R`.

## Testing

Characterisation tests on one real FIT file can only show the code ran. The suite instead builds sources with **known** injected distortions via `tests/testthat/helper-synthetic.R` (`make_source()` for gain/offset/lag/noise, `make_level_dependent()` for a bias that changes across the range, `featureless_rides()` for the unidentifiable-offset case) and asserts the machinery recovers the planted number. When adding a capability, plant a known answer for it rather than pinning whatever the code currently emits.

Two live traps:

- Anything using the bootstrap must be wrapped in `withr::with_seed()` or it is flaky.
- A *pure* gain with no noise gives every second an identical log ratio, so the bootstrap distribution is degenerate and the interval collapses to a point. That is correct, not a bug — add noise when testing that an interval brackets a value.

The Coggan tests assert the definitions analytically — a steady effort has NP equal to AP, an hour at FTP scores exactly 100 TSS — rather than pinning fixture output. Prefer extending that pattern; the bundled ride has only 2 zeros in 3,601 seconds, so it barely exercises coasting behaviour at all.

## Metrics and simulation

`coggan_formulas.r` follows the published Coggan definitions: NP rolls a 30-second *trailing* mean over the whole recording with coasting counted as zero, and AP is the mean over the full duration. `drop_zeros()` remains exported for composition (`average_power(drop_zeros(x))`) but nothing internal uses it. NP returns `NA` below 30 seconds.

One subtlety that looks like a bug and is not: NP is computed over the rolling means (seconds 30 onward) while AP covers the whole series, so `NP >= AP` can fail marginally on near-constant data. Independent noise has VI ≈ 1 because the 30-second window averages scatter away.

That last fact is why `simulate_power()` works the way it does. It does **not** optimise over the series — that was 3,600 unknowns against 2 constraints, starting from white noise that structurally cannot produce VI > 1. Instead it exploits both metrics being homogeneous of degree 1 in power, so VI is scale-free: an autocorrelated shape is raised to an exponent solved by 1-D `uniroot` to set VI, then multiplied through to set AP. Both targets come out exact, and convergence is deterministic given a seed. If you change `normalised_power()`, this solve follows it automatically — but check `solve_spikiness()` still brackets, since it relies on VI being exactly 1 at exponent 0 and monotone above.
