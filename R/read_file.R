#' Read a ride file
#'
#' Reads a `.fit`, `.gpx` or `.csv` file into the canonical ride schema, so
#' nothing downstream has to care which format a ride arrived in.
#'
#' @param file_url Path or URL of a `.fit`, `.gpx` or `.csv` file. A CSV needs a
#'   `time` column, in ISO 8601 or epoch seconds, and a `power` column.
#'
#' @return A tibble with `time` (POSIXct, UTC) and `power` (watts), plus
#'   whichever of `cadence`, `heart_rate`, `speed`, `distance`, `altitude`,
#'   `temperature`, `lat` and `lon` the file carries, ordered by time.
#'
#' @examples
#' read_file(system.file("extdata", "power1.fit", package = "cyclingpower"))
#' @export
read_file <- function(file_url) {
  file_ext <- tolower(tools::file_ext(file_url))

  switch(
    file_ext,
    fit = read_fit(file_url),
    gpx = read_gpx(file_url),
    csv = read_csv_ride(file_url),
    stop(sprintf("%s is not a FIT, GPX or CSV file.", basename(file_url)))
  )
}

#' FIT files store records against one or more message definitions, so a ride
#' can arrive as several tibbles with different columns.
#' @noRd
read_fit <- function(file_url) {
  if (!requireNamespace("FITfileR", quietly = TRUE)) {
    stop(
      "Reading FIT files needs FITfileR, which is not on CRAN. Install it ",
      'with remotes::install_github("grimbough/FITfileR").'
    )
  }

  FITfileR::readFitFile(file_url) |>
    FITfileR::records() |>
    dplyr::bind_rows() |>
    canonicalise(file_url)
}
