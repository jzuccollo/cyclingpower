#' Read several ride files into one long tibble
#'
#' The unit of analysis is a run (one session, ridden once) recorded by two or
#' more sources (power meters). This assembles that structure explicitly, so
#' the `run` and `source` labels come from a spec you can read.
#'
#' A directory is accepted as well as a spec, since a folder of files is what
#' you usually have. The filenames are then read by [ride_spec_from_names()],
#' which needs them to be `<run>_<source>`; build the spec yourself where they
#' are not.
#'
#' @param spec Data frame with columns `path`, `run` and `source`, or the path
#'   to a directory of `.fit`, `.gpx` or `.csv` files named `<run>_<source>`.
#'
#' @return A tibble on the canonical ride schema with `run` and `source`
#'   prepended.
#'
#' @family reading
#' @examples
#' rides <- read_rides(demo_rides())
#' head(rides)
#' @export
read_rides <- function(spec) {
  spec <- as_ride_spec(spec)

  missing <- setdiff(c("path", "run", "source"), names(spec))
  if (length(missing) > 0) {
    stop(
      "spec needs columns ",
      paste(missing, collapse = ", "),
      ". See ride_spec_from_names() to build one from filenames."
    )
  }

  Map(
    function(path, run, source) {
      read_file(path) |>
        dplyr::mutate(run = run, source = source, .before = 1)
    },
    spec$path,
    spec$run,
    spec$source
  ) |>
    dplyr::bind_rows()
}

#' Build a ride spec from filenames
#'
#' Assumes files are named `<run><sep><source>`, so `run1_kickr.fit` is the
#' kickr's recording of run 1. A filename that does not split into exactly two
#' parts is an error rather than a silent `NA`, which is the failure mode that
#' previously left every ride labelled with the same missing source.
#'
#' @param files Character vector of file paths.
#' @param sep Separator between run and source in the filename.
#'
#' @return A tibble with `path`, `run` and `source`, ready for `read_rides()`.
#'
#' @family reading
#' @examples
#' ride_spec_from_names(c("run1_kickr.fit", "run1_4iiii.fit"))
#' @export
ride_spec_from_names <- function(files, sep = "_") {
  parts <- tools::file_path_sans_ext(basename(files)) |>
    strsplit(sep, fixed = TRUE)

  bad <- lengths(parts) != 2
  if (any(bad)) {
    stop(
      "These filenames do not split into run",
      sep,
      "source: ",
      paste(basename(files[bad]), collapse = ", ")
    )
  }

  tibble::tibble(
    path = files,
    run = vapply(parts, `[`, character(1), 1),
    source = vapply(parts, `[`, character(1), 2)
  )
}

#' A directory becomes a spec; a spec passes through. Kept separate so
#' [read_rides()] and [compare_report()] resolve a directory the same way.
#' @noRd
as_ride_spec <- function(spec) {
  if (is.character(spec) && length(spec) == 1 && dir.exists(spec)) {
    ride_spec_from_names(ride_files(spec))
  } else {
    spec
  }
}

#' The ride files in a directory.
#'
#' Shared by [read_rides()] and the app's Rides tab so the two cannot disagree
#' about what counts as a ride file.
#' @noRd
ride_files <- function(dir) {
  list.files(
    dir,
    pattern = "\\.(fit|gpx|csv)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
}
