#' Write a set of demonstration ride files
#'
#' Builds four CSVs describing two sessions and three meters, with distortions
#' chosen so every part of the comparison has something to show. Useful for
#' trying the package, and for [launch_app()]'s demo button.
#'
#' The meters are named after real ones only to make the naming convention
#' concrete. What they do is planted:
#'
#' - `assioma` is the reference and appears in both runs, so it is the bridge.
#' - `kickr` reads about 2.5% below it, the difference a drivetrain makes
#'   between the pedals and a trainer. Its clock also runs 7 seconds ahead, so
#'   the alignment has an offset to find.
#' - `quarq` reads low at endurance pace and high above 200 W, the pattern a
#'   single average difference hides and [agreement_by_level()] exists to show.
#'
#' `kickr` and `quarq` never ride together, so their comparison has to be
#' chained through `assioma`.
#'
#' A little noise is added to every meter. Without it a pure gain gives every
#' second an identical log ratio, the bootstrap distribution is degenerate, and
#' the intervals collapse to a point — correct, but it reads as a bug.
#'
#' @param dir Directory to write into. Created if it does not exist.
#'
#' @return The directory path, invisibly.
#'
#' @examples
#' dir <- demo_rides(tempfile())
#' list.files(dir)
#' @export
demo_rides <- function(dir = file.path(tempdir(), "cyclingpower-demo")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  withr::with_seed(1, {
    run1 <- simulate_power(1800, target_ap = 190, target_np = 215)
    run2 <- simulate_power(1800, target_ap = 205, target_np = 240)

    write_demo_ride(dir, "run1", "assioma", run1, 0)
    write_demo_ride(dir, "run1", "kickr", run1 * 0.975, 7)
    write_demo_ride(dir, "run2", "assioma", run2, 0)
    write_demo_ride(dir, "run2", "quarq", level_dependent(run2), 0)
  })

  invisible(dir)
}

#' Agreement at endurance pace and a widening gap above it, which the overall
#' average reports as a small constant bias.
#' @noRd
level_dependent <- function(power, threshold = 200) {
  power * ifelse(power < threshold, 0.97, 1.05)
}

#' @noRd
write_demo_ride <- function(dir, run, source, power, clock_offset) {
  start <- as.POSIXct("2026-03-14 09:00:00", tz = "UTC")
  power <- pmax(0, power + stats::rnorm(length(power), 0, 4))

  path <- file.path(dir, paste0(run, "_", source, ".csv"))
  utils::write.csv(
    data.frame(
      time = format(
        start + seq_along(power) - 1 + clock_offset,
        "%Y-%m-%dT%H:%M:%SZ"
      ),
      power = round(power, 1)
    ),
    path,
    row.names = FALSE
  )
  path
}
