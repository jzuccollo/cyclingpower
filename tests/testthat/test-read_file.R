ext <- function(f) system.file("extdata", f, package = "cyclingpower")

test_that("FIT files read onto the canonical schema", {
  ride <- read_file(ext("power1.fit"))

  expect_s3_class(ride, "tbl_df")
  expect_true(all(c("time", "power") %in% names(ride)))
  expect_s3_class(ride$time, "POSIXct")
  expect_type(ride$power, "double")
  expect_equal(nrow(ride), 3601)
  expect_false(anyNA(ride$power))
  expect_false(is.unsorted(ride$time))
})

test_that("readers agree on column names and types across formats", {
  fit <- read_file(ext("power1.fit"))
  gpx <- read_file(ext("power_strava.gpx"))

  shared <- intersect(names(fit), names(gpx))
  expect_true(all(c("time", "power") %in% shared))
  expect_equal(
    vapply(fit[shared], function(x) class(x)[[1]], character(1)),
    vapply(gpx[shared], function(x) class(x)[[1]], character(1))
  )
})

test_that("power is found under either GPX spelling", {
  strava <- read_file(ext("power_strava.gpx"))
  garmin <- read_file(ext("power_garmin.gpx"))

  expect_equal(strava$power, c(210, 224, 197))
  expect_equal(strava, garmin)
})

test_that("a GPX with no power channel errors rather than returning NA", {
  expect_error(read_file(ext("nopower.gpx")), "No power data found")
})

test_that("unsupported extensions are rejected", {
  expect_error(read_file("ride.tcx"), "not a FIT, GPX or CSV file")
})

test_that("CSV files read onto the same canonical schema", {
  csv <- read_file(ext("power_recorded.csv"))
  fit <- read_file(ext("power1.fit"))

  expect_s3_class(csv, "tbl_df")
  expect_s3_class(csv$time, "POSIXct")
  expect_equal(attr(csv$time, "tzone"), "UTC")
  expect_type(csv$power, "double")
  expect_equal(nrow(csv), 120)

  # The whole point of the schema layer: a reader may not invent its own names
  # or types, and anything outside canonical_columns() is dropped rather than
  # carried downstream. `device` is in the fixture precisely to be discarded.
  expect_false("device" %in% names(csv))
  shared <- intersect(names(csv), names(fit))
  expect_true(all(c("time", "power", "cadence") %in% shared))
  expect_equal(
    vapply(csv[shared], function(x) class(x)[[1]], character(1)),
    vapply(fit[shared], function(x) class(x)[[1]], character(1))
  )
})

test_that("CSV time is read as UTC from either spelling", {
  iso <- read_file(ext("power_recorded.csv"))

  epoch <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(time = as.numeric(iso$time), power = iso$power),
    epoch,
    row.names = FALSE
  )

  expect_equal(read_file(epoch)$time, iso$time)
})

test_that("a CSV with no power channel errors rather than returning NA", {
  expect_error(read_file(ext("nopower.csv")), "No power data found")
})

test_that("a CSV with no time column says so", {
  no_time <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(power = c(200, 210)), no_time, row.names = FALSE)

  expect_error(read_file(no_time), "No time column")
})

test_that("ride specs from filenames fail loudly on the wrong shape", {
  spec <- ride_spec_from_names(c("run1_kickr.fit", "run1_4iiii.fit"))
  expect_equal(spec$run, c("run1", "run1"))
  expect_equal(spec$source, c("kickr", "4iiii"))

  # The old vignette split names that had no separator and silently produced
  # an all-NA source column, leaving every ride labelled identically.
  expect_error(ride_spec_from_names(c("run1.fit", "run2.fit")), "do not split")
})
