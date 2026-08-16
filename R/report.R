#' Run every comparison over one set of aligned rides
#'
#' The single computation path. Both the report and the Shiny app present these
#' results rather than each running the pipeline themselves, which is what stops
#' the two drifting into disagreeing about the same rides.
#'
#' Chaining is attempted rather than assumed: a set of rides with no meter in
#' more than one run has nothing to chain through, which is an ordinary state
#' rather than a failure, so the error is captured into `chain_error` and the
#' rest of the results stand.
#'
#' @param aligned Result of [align_rides()].
#' @param spec The spec the rides were read from, carried through for reporting.
#' @param bridge Source common to more than one run, passed to [chain_sources()].
#' @inheritParams agreement_by_level
#'
#' @return A named list holding `aligned`, `spec`, `alignment`, `agreement`,
#'   `by_level`, `chained`, `chain`, `chain_error` and the `settings` used.
#'   `chained` and `chain` are `NULL` when chaining was not possible.
#' @noRd
compare_all <- function(
  aligned,
  spec = NULL,
  min_power = 0,
  width = 25,
  min_seconds = 30,
  block = 60,
  replicates = 1000,
  bridge = NULL,
  seed = NULL,
  max_offset = NULL,
  smooth = 30
) {
  chained <- try(
    chain_sources(aligned, bridge, min_power, block, replicates),
    silent = TRUE
  )
  failed <- inherits(chained, "try-error")

  list(
    aligned = aligned,
    spec = spec,
    alignment = alignment_report(aligned),
    agreement = power_agreement(aligned, min_power, block, replicates),
    by_level = agreement_by_level(
      aligned,
      width = width,
      min_seconds = min_seconds,
      min_power = min_power,
      block = block,
      replicates = replicates
    ),
    chained = if (failed) NULL else chained,
    chain = if (failed) NULL else chain_report(chained),
    chain_error = if (failed) conditionMessage(attr(chained, "condition")),
    settings = list(
      min_power = min_power,
      width = width,
      min_seconds = min_seconds,
      block = block,
      replicates = replicates,
      seed = seed,
      max_offset = max_offset,
      smooth = smooth
    )
  )
}

#' Render the whole comparison from a set of files
#'
#' The one-call entry point: hand it a spec of ride files and it reads them,
#' puts them on a common clock, and reports how the meters compare overall,
#' across the power range, and — where one meter is common to more than one
#' run — between meters that never rode together.
#'
#' @param spec Data frame with columns `path`, `run` and `source`, as built by
#'   [ride_spec_from_names()], or the path to a directory of ride files named
#'   `<run>_<source>`.
#' @param output_file Path of the HTML file to write.
#' @param min_power Both sources must exceed this to count, in watts.
#' @param width Bin width for the power-range breakdown, in watts.
#' @param min_seconds Power bands with fewer paired seconds than this are
#'   dropped.
#' @param block Bootstrap block length in seconds.
#' @param replicates Number of bootstrap replicates.
#' @param max_offset Largest clock offset to consider, in seconds.
#' @param smooth Window of the rolling mean on the power traces, in seconds.
#' @param seed Seed for the bootstrap. The intervals are resampled, so two runs
#'   over the same rides differ slightly unless this is set.
#'
#' @return The path of the rendered file, invisibly.
#'
#' @examplesIf rlang::is_installed("rmarkdown") && rmarkdown::pandoc_available()
#' compare_report(demo_rides(), tempfile(fileext = ".html"))
#' @export
compare_report <- function(
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
) {
  spec <- as_ride_spec(spec)
  aligned <- read_rides(spec) |> align_rides(max_offset = max_offset)

  compute <- function() {
    compare_all(
      aligned,
      spec,
      min_power,
      width,
      min_seconds,
      block,
      replicates,
      seed = seed,
      max_offset = max_offset,
      smooth = smooth
    )
  }

  results <- if (is.null(seed)) compute() else withr::with_seed(seed, compute())

  render_comparison(results, output_file)
}

#' Render a set of results to HTML
#'
#' The presentation half of [compare_report()], split out because the app
#' already holds computed results and re-running the bootstrap to render them
#' would both waste the wait and give the document a second chance to disagree
#' with the screen.
#'
#' @param results A results list, as built by `compare_all()`.
#' @param output_file Path of the HTML file to write.
#'
#' @return The path of the rendered file, invisibly.
#' @noRd
render_comparison <- function(results, output_file) {
  rlang::check_installed("rmarkdown", "to render the comparison report.")

  installed <- system.file(
    "rmarkdown",
    "comparison.Rmd",
    package = "cyclingpower"
  )

  # Rendered from a copy, because knitr writes comparison.knit.md and the figure
  # files beside the template it is given. Pointed at the installed one, that is
  # a write into the package's own directory, which fails outright for anyone
  # whose library is read-only.
  template <- file.path(withr::local_tempdir(), basename(installed))
  file.copy(installed, template)

  rendered <- rmarkdown::render(
    template,
    output_file = basename(output_file),
    output_dir = dirname(normalizePath(output_file, mustWork = FALSE)),
    params = list(results = results),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  invisible(rendered)
}
