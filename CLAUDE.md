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

`FITfileR` is GitHub-only (`remotes::install_github("grimbough/FITfileR")`) and sits in `Suggests` with a `Remotes:` entry. Anything touching a FIT file fails without it; the `compare_pms` vignette guards on it with `knitr::opts_chunk$set(eval = has_fit)`.

## Architecture

A five-layer pipeline. Each layer's output is the next layer's input, and the layers are independently testable.

**Ingest** (`read_file.R`, `read_gpx.R`, `schema.R`, `read_rides.R`) — `read_file()` dispatches on extension and both paths funnel through `canonicalise()`, which renames format-specific aliases and enforces one schema: `time` (POSIXct, UTC) and `power` mandatory, plus optional `cadence`, `heart_rate`, `speed`, `distance`, `altitude`, `temperature`, `lat`, `lon`. **If you add a channel or a reader, add it to `canonical_columns()`/`column_aliases()` rather than special-casing downstream** — the FIT and GPX paths previously drifted into emitting `timestamp` and `time` respectively, which is what the schema layer exists to prevent.

`read_gpx.R` matches on XPath `local-name()` rather than namespace prefixes, so it finds power whether written as Strava's bare `<power>` or Garmin's `gpxpx:PowerInWatts`. A file with no power channel is an error, not an `NA` column.

`read_rides(spec)` takes an explicit `path`/`run`/`source` data frame. The `run` (one session) and `source` (one meter) columns key everything downstream.

**Align** (`align.R`) — the layer the rest depends on being correct. Resamples each source onto a 1 Hz grid relative to the run's earliest sample, averaging duplicate seconds (without which the later `pivot_wider` fans out rows), then estimates each source's clock offset against a within-run reference by cross-correlating power over ±`max_offset`.

Sign convention: `best_lag()` finds `k` maximising `cor(reference[i], candidate[i + k])`, so a positive `k` means the candidate's timestamps run `k` seconds *ahead* and the correction is `t - k`. `test-align.R` pins this in both directions; a sign flip here silently corrupts everything.

`check_alignment()` deliberately collapses "not the same ride" and "steady effort with no features to lock onto" into one `unresolved` status. They cannot be separated by correlation alone, and erroring would block steady trainer sessions, which are the package's main use case. Unresolved means the offset is forced to 0 and a warning fires; the numbers are in `alignment_report()`.

**Compare** (`agreement.R`, `bootstrap.R`) — `paired_seconds()` is the shared workhorse: for each run it pivots wide, forms every source pair via `combn`, and returns one row per pair per second with `difference`, `level` (the pair mean) and `log_ratio`. Coasting seconds are dropped.

Bland-Altman throughout: `level` is the mean of the two meters, never one nominated meter, because binning on one meter and plotting its own difference against that bin turns measurement noise into an apparent slope.

All intervals come from `block_bootstrap_draws()` (moving block, default 60 s). Per-second power is heavily autocorrelated, so an i.i.d. interval is roughly an order of magnitude too narrow. `test-agreement.R` asserts the bootstrap interval is more than 3× the naive one — if that test starts passing trivially, the bootstrap has broken.

**Chain** (`chain.R`) — links meters that never rode together. `resolve_bridge()` finds the source present in more than one run; `bridge_pairs()` orients every log ratio as `log(source / bridge)`, flipping sign where the bridge sorts first alphabetically. `pool_by_source()` combines a source appearing against the bridge in several runs, weighted by seconds — needed as soon as the same pair is ridden twice.

Runs are bootstrapped independently and the draws subtracted, so a chained interval is necessarily wider than the direct ones it is built from. The `basis` column (`direct`/`chained`) is load-bearing: chained comparisons rest on the bridge reading identically on both days, which the data cannot test. `chain_report()$assumption` says so explicitly — keep that in any output.

**Visualise and report** (`comparison_charts.r`, `report.R`, `inst/rmarkdown/comparison.Rmd`) — plot functions take the thing they visualise: `plot_timeseries()`/`plot_agreement()` take aligned rides, `plot_by_level()` takes `agreement_by_level()` output, `plot_chain()` takes `chain_sources()` output. `compare_report()` renders the parameterised Rmd.

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
