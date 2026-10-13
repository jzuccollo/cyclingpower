#' GPX puts everything beyond position and time in `<extensions>`, with no
#' single spelling: Strava writes a bare `<power>`, Garmin's PowerExtension
#' writes `<gpxpx:PowerInWatts>`, and heart rate and cadence arrive as
#' `<gpxtpx:hr>` and `<gpxtpx:cad>`.
#' @noRd
gpx_field_names <- function() {
  list(
    time = "time",
    power = c("power", "PowerInWatts", "watts"),
    cadence = c("cad", "cadence"),
    heart_rate = c("hr", "heartrate", "heart_rate"),
    speed = "speed",
    distance = "distance",
    altitude = c("ele", "altitude"),
    temperature = c("atemp", "temp", "temperature")
  )
}

#' Matched on local name, not namespace prefix. Prefixes are arbitrary and the
#' URIs differ across GPX versions and Garmin's several extension schemas, so
#' keying on them means an unexpected namespace silently yields nothing.
#' @noRd
local_name_xpath <- function(names) {
  paste0(
    ".//*[",
    paste0("local-name()='", names, "'", collapse = " or "),
    "]"
  )
}

#' GPX carries ISO 8601, usually with a trailing `Z` but sometimes a numeric
#' offset. Normalise the zone designator into the `+hhmm` form strptime reads.
#' @noRd
parse_gpx_time <- function(x) {
  x <- sub("Z$", "+0000", trimws(x))
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"), tz = "UTC")
}

#' Replaces `trackeR::readGPX()`, which we used for this one job at the cost
#' of a dozen transitive dependencies, and which finds power only when it is
#' written as a bare `<power>` in the default GPX namespace.
#' @noRd
read_gpx <- function(path) {
  points <- xml2::read_xml(path) |>
    xml2::xml_find_all("//*[local-name()='trkpt']")

  if (length(points) == 0) {
    stop(sprintf("No track points found in %s.", basename(path)))
  }

  extracted <- lapply(gpx_field_names(), function(names) {
    xml2::xml_text(xml2::xml_find_first(points, local_name_xpath(names)))
  })

  out <- data.frame(extracted, stringsAsFactors = FALSE)
  out$lat <- xml2::xml_attr(points, "lat")
  out$lon <- xml2::xml_attr(points, "lon")

  # Drop channels the file never populated, so a missing power channel reaches
  # canonicalise() as an absent column rather than a column of NA.
  out <- out[, vapply(out, function(col) !all(is.na(col)), logical(1))]
  out$time <- parse_gpx_time(out$time)

  canonicalise(out, path)
}
