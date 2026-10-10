#' Every reader returns this shape, whatever the container format.
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
#' altitude and speed.
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

  numeric_cols <- setdiff(names(x), "time")
  x[numeric_cols] <- lapply(x[numeric_cols], function(col) {
    suppressWarnings(as.numeric(col))
  })

  # After coercion, not before: a GPX with an empty <power/> on every trackpoint
  # has a power column made entirely of "", which is present and not NA but
  # empty. Checking the raw column passes it through as an all-NA
  # numeric that fails much later with nothing to point at.
  if (!"power" %in% names(x) || all(is.na(x$power))) {
    abort_no_power(file)
  }

  x <- x[order(x$time), , drop = FALSE]
  tibble::as_tibble(x)
}

#' The usual cause is not a corrupt file but an activity whose power Strava
#' estimated rather than recorded.
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
