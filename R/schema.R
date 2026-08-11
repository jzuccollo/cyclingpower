#' Every reader returns this shape, whatever the container format. Keeping it
#' in one place is what stops the FIT and GPX paths drifting apart, which is
#' how the FIT reader came to emit `timestamp` while the GPX reader emitted
#' `time`.
#' @noRd
canonical_columns <- function() {
  c(
    "time",
    "power",
    "cadence",
    "heart_rate",
    "speed",
    "distance",
    "altitude",
    "temperature",
    "lat",
    "lon"
  )
}

#' Devices vary in whether they write the plain or `enhanced_` variant of
#' altitude and speed, and FIT names things after the FIT profile.
#' @noRd
column_aliases <- function() {
  c(
    timestamp = "time",
    position_lat = "lat",
    position_long = "lon",
    enhanced_altitude = "altitude",
    enhanced_speed = "speed",
    heartrate = "heart_rate",
    watts = "power"
  )
}

#' @noRd
canonicalise <- function(x, file) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  aliases <- column_aliases()
  hit <- match(names(x), names(aliases))
  names(x)[!is.na(hit)] <- aliases[stats::na.omit(hit)]

  x <- x[, intersect(canonical_columns(), names(x)), drop = FALSE]

  if (!"power" %in% names(x) || all(is.na(x$power))) {
    abort_no_power(file)
  }

  numeric_cols <- setdiff(names(x), "time")
  x[numeric_cols] <- lapply(x[numeric_cols], as.numeric)

  x <- x[order(x$time), , drop = FALSE]
  tibble::as_tibble(x)
}

#' Worth a specific message: the usual cause is not a corrupt file but an
#' activity whose power Strava estimated rather than recorded.
#' @noRd
abort_no_power <- function(file) {
  stop(
    sprintf(
      paste0(
        "No power data found in %s.\n",
        "Exports only carry power a meter actually recorded. Power that Strava ",
        "estimated from speed and gradient is not included, so a ride done ",
        "without a meter reads as having no power.\n",
        "Where you have the choice, prefer the original FIT file over a GPX."
      ),
      basename(file)
    )
  )
}
