#' Read a ride file
#'
#' Reads a `.fit`, `.gpx` or `.csv` file into the canonical ride schema, so
#' nothing downstream has to care which format a ride arrived in.
#'
#' @param path Path or URL of a `.fit`, `.gpx` or `.csv` file. A CSV needs a
#'   `time` column, in ISO 8601 or epoch seconds, and a `power` column.
#'
#' @return A tibble with `time` (POSIXct, UTC) and `power` (watts), plus
#'   whichever of `cadence`, `heart_rate`, `speed`, `distance`, `altitude`,
#'   `temperature`, `lat` and `lon` the file carries, ordered by time.
#'
#' @family reading
#' @examples
#' read_file(system.file("extdata", "power_strava.gpx", package = "cyclingpower"))
#'
#' # FIT needs FITfileR, which is not on CRAN
#' @examplesIf requireNamespace("FITfileR", quietly = TRUE)
#' read_file(system.file("extdata", "power1.fit", package = "cyclingpower"))
#' @export
read_file <- function(path) {
  file_ext <- tolower(tools::file_ext(path))

  switch(
    file_ext,
    fit = read_fit(path),
    gpx = read_gpx(path),
    csv = read_csv_ride(path),
    stop(sprintf("%s is not a FIT, GPX or CSV file.", basename(path)))
  )
}

#' FIT files store records against one or more message definitions, so a ride
#' can arrive as several tibbles with different columns.
#' @noRd
read_fit <- function(path) {
  if (!requireNamespace("FITfileR", quietly = TRUE)) {
    stop(
      "Reading FIT files needs FITfileR, which is not on CRAN. Install it ",
      'with remotes::install_github("grimbough/FITfileR").'
    )
  }

  FITfileR::readFitFile(path) |>
    FITfileR::records() |>
    dplyr::bind_rows() |>
    canonicalise(path)
}
