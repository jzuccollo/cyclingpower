#' The format the recorders in this package write, and the easiest thing to
#' hand-assemble. Everything format-specific stays here; `canonicalise()` does
#' the renaming and the schema check, as it does for FIT and GPX.
#' @noRd
read_csv_ride <- function(path) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE)

  if (!"time" %in% names(out)) {
    stop(
      sprintf("No time column found in %s.", basename(path)),
      " A ride CSV needs `time` and `power` at minimum."
    )
  }

  # Drop channels the file never populated, so a missing power channel reaches
  # canonicalise() as an absent column rather than a column of NA.
  out <- out[, vapply(out, function(col) !all(is.na(col)), logical(1))]
  out$time <- parse_csv_time(out$time)

  canonicalise(out, path)
}

#' Two spellings: ISO 8601, which is what a recorder should write, and epoch
#' seconds, which is what falls out of a script that never bothered to format
#' the clock.
#' @noRd
parse_csv_time <- function(x) {
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }

  parsed <- parse_gpx_time(x)
  if (all(is.na(parsed))) {
    parsed <- as.POSIXct(trimws(x), tz = "UTC")
  }
  parsed
}
