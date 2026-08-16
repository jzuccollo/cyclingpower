#' Open a set of ride files for recording
#'
#' Opens one CSV per source, named `<run><sep><source>.csv`, and returns a
#' handle to write samples into. The files are the canonical ride schema —
#' `time` and `power` — so [read_file()] reads back what was written and
#' [ride_spec_from_names()] parses the run and source straight out of the
#' filenames.
#'
#' Samples are flushed to disk as they arrive rather than held and written at
#' the end, so a browser tab that dies mid-ride costs the last fraction of a
#' second rather than the whole recording. Analysis is repeatable and recording
#' is not, which is the argument for the whole arrangement.
#'
#' @param dir Directory to write into. Created if it does not exist.
#' @param run Label for this session, shared by every file.
#' @param sources Character vector of meter labels, one file each.
#'
#' @return An object of class `power_recording`, to pass to
#'   `record_samples()` and then `stop_recording()`.
#'
#' @examples
#' dir <- tempfile()
#' recording <- start_recording(dir, "run1", c("kickr", "assioma"))
#' recording <- record_samples(
#'   recording,
#'   data.frame(
#'     source = c("kickr", "assioma"),
#'     time = "2026-03-01T09:00:00.000Z",
#'     power = c(198, 204)
#'   )
#' )
#' stop_recording(recording)
#' @noRd
start_recording <- function(dir, run, sources) {
  check_label(run, "A run")
  for (source in sources) {
    check_label(source, "A source")
  }
  if (anyDuplicated(sources) > 0) {
    stop(
      "Two meters cannot share a source label: they would write to one file."
    )
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, paste0(run, "_", sources, ".csv"))

  # A genuine boundary. Appending today's ride to a file of the same name from
  # last week produces one file spanning two sessions, which reads as a single
  # ride with an enormous gap in the middle and nothing to say otherwise.
  taken <- paths[file.exists(paths)]
  if (length(taken) > 0) {
    stop(
      "These files already exist: ",
      paste(basename(taken), collapse = ", "),
      ". Use a different run name rather than writing over a recording."
    )
  }

  connections <- lapply(paths, function(path) {
    con <- file(path, open = "a")
    writeLines("time,power", con)
    flush(con)
    con
  })
  names(connections) <- sources

  structure(
    list(
      dir = dir,
      run = run,
      sources = sources,
      paths = stats::setNames(paths, sources),
      connections = connections,
      rows = stats::setNames(integer(length(sources)), sources)
    ),
    class = "power_recording"
  )
}

#' Append samples to an open recording
#'
#' Writes each sample as it arrives and flushes, so what is on disk is never
#' more than one batch behind what the meters have sent.
#'
#' A sample is written only when one arrives. A meter that drops out leaves a
#' gap, which [align_rides()] resamples over and `paired_seconds()` has no row
#' for. Padding the gap with zeros instead would enter the dropout into the
#' comparison as genuine coasting, where it is dropped as a second both meters
#' agreed on — turning a lost connection into evidence of agreement, which
#' nothing downstream could then recover from.
#'
#' @param recording A `power_recording` from `start_recording()`.
#' @param samples Data frame with `source`, `time` and `power`. The `time`
#'   values are written through unaltered, so the recorder's clock is what
#'   reaches the file.
#'
#' @return The recording, with its row counts updated.
#'
#' @noRd
record_samples <- function(recording, samples) {
  if (nrow(samples) == 0) {
    return(recording)
  }

  for (source in intersect(recording$sources, unique(samples$source))) {
    rows <- samples[samples$source == source, ]
    con <- recording$connections[[source]]
    writeLines(paste0(rows$time, ",", rows$power), con)
    flush(con)
    recording$rows[[source]] <- recording$rows[[source]] + nrow(rows)
  }

  recording
}

#' Close a recording
#'
#' @param recording A `power_recording` from `start_recording()`.
#'
#' @return A tibble of `run`, `source`, `path` and `rows` written — the first
#'   three being the spec [read_rides()] wants.
#'
#' @noRd
stop_recording <- function(recording) {
  for (con in recording$connections) {
    close(con)
  }

  tibble::tibble(
    run = recording$run,
    source = recording$sources,
    path = unname(recording$paths),
    rows = unname(as.integer(recording$rows))
  )
}

#' The labels become filenames that `ride_spec_from_names()` splits on the
#' underscore, so a label carrying one silently produces a spec with the run and
#' source in the wrong places — or, more often, a file the scan refuses
#' wholesale. Cheaper to refuse the label than to explain the scan.
#' @noRd
check_label <- function(x, what) {
  if (length(x) != 1 || is.na(x) || !grepl("^[A-Za-z0-9][A-Za-z0-9-]*$", x)) {
    stop(
      what,
      " label becomes part of a filename that is read back as ",
      "<run>_<source>, so it can hold letters, digits and hyphens only. ",
      "Cannot use: ",
      if (length(x) == 1) x else "an empty label",
      "."
    )
  }
}
