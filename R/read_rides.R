#' Read several ride files into one long tibble
#'
#' The unit of analysis is a run (one session, ridden once) recorded by two or
#' more sources (power meters). This assembles that structure explicitly, so
#' the `run` and `source` labels come from a spec you can read rather than
#' from parsing filenames and hoping.
#'
#' @param spec Data frame with columns `path`, `run` and `source`.
#'
#' @return A tibble on the canonical ride schema with `run` and `source`
#'   prepended.
#'
#' @examples
#' \dontrun{
#' spec <- data.frame(
#'   path = c("run1_kickr.fit", "run1_4iiii.fit"),
#'   run = "run1",
#'   source = c("kickr", "4iiii")
#' )
#' read_rides(spec)
#' }
#' @export
read_rides <- function(spec) {
  missing <- setdiff(c("path", "run", "source"), names(spec))
  if (length(missing) > 0) {
    stop(
      "spec needs columns ",
      paste(missing, collapse = ", "),
      ". See ride_spec_from_names() to build one from filenames."
    )
  }

  purrr::pmap(
    list(spec$path, spec$run, spec$source),
    function(path, run, source) {
      read_file(path) |>
        dplyr::mutate(run = run, source = source, .before = 1)
    }
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
