#' Pair up every second two sources both recorded
#'
#' Coasting seconds are dropped: when both meters read zero they agree
#' trivially, which drags the mean difference towards zero and inflates the
#' count of seconds the interval is built from. Neither meter is being asked
#' to measure anything, so the second carries no information about how they
#' compare.
#'
#' @param aligned Result of [align_rides()].
#' @param min_power Both sources must exceed this to count, in watts.
#'
#' @return Long tibble with one row per pair per second.
#' @noRd
paired_seconds <- function(aligned, min_power = 0) {
  runs <- split(aligned, aligned$run)

  lapply(runs, function(ride) {
    sources <- sort(unique(ride$source))
    if (length(sources) < 2) {
      return(NULL)
    }

    wide <- ride |>
      dplyr::select("t", "source", "power") |>
      tidyr::pivot_wider(names_from = "source", values_from = "power")

    utils::combn(sources, 2, simplify = FALSE) |>
      lapply(function(pair) {
        tibble::tibble(
          run = ride$run[[1]],
          source_a = pair[[1]],
          source_b = pair[[2]],
          t = wide$t,
          a = wide[[pair[[1]]]],
          b = wide[[pair[[2]]]]
        )
      }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(
      !is.na(.data$a),
      !is.na(.data$b),
      .data$a > min_power,
      .data$b > min_power
    ) |>
    dplyr::mutate(
      difference = .data$a - .data$b,
      level = (.data$a + .data$b) / 2,
      log_ratio = log(.data$a / .data$b)
    )
}

#' How far apart two meters read
#'
#' Reports the Bland-Altman summary for every pair of sources in every run:
#' the mean difference, the spread of that difference, and the limits of
#' agreement within which most seconds fall. The gain is the same thing on a
#' multiplicative scale, which is usually the more natural way to read a power
#' meter's error, since meters tend to be a few per cent out rather than a
#' fixed number of watts out.
#'
#' Intervals come from a moving-block bootstrap, not from treating each second
#' as independent.
#'
#' @param aligned Result of [align_rides()].
#' @param min_power Both sources must exceed this to count, in watts.
#' @param block Bootstrap block length in seconds.
#' @param replicates Number of bootstrap replicates.
#'
#' @return A tibble with one row per source pair per run:
#'   \describe{
#'     \item{`run`, `source_a`, `source_b`}{The pair and the session it came
#'       from. `source_a` sorts before `source_b` alphabetically, and every
#'       difference is `source_a` minus `source_b`.}
#'     \item{`seconds`}{Paired seconds behind the row, after coasting and
#'       anything below `min_power` is dropped.}
#'     \item{`bias_w`, `bias_lo`, `bias_hi`}{Mean difference in watts, with its
#'       95% bootstrap interval.}
#'     \item{`sd_difference`}{Standard deviation of the per-second difference.}
#'     \item{`loa_lo`, `loa_hi`}{Limits of agreement, `bias_w` +/- 1.96 SD:
#'       the range most individual seconds fall within. Not an interval on the
#'       mean, and much wider than one.}
#'     \item{`gain_pc`, `gain_lo`, `gain_hi`}{The same difference
#'       multiplicatively, as a percentage, with its 95% bootstrap interval.
#'       Usually the more natural reading.}
#'   }
#'
#' @family agreement
#' @examples
#' aligned <- read_rides(demo_rides()) |> align_rides()
#' power_agreement(aligned)
#' @export
power_agreement <- function(
  aligned,
  min_power = 0,
  block = 60,
  replicates = 1000
) {
  pairs <- paired_seconds(aligned, min_power)

  out <- pairs |>
    dplyr::group_by(.data$run, .data$source_a, .data$source_b) |>
    dplyr::group_modify(~ agreement_summary(.x, block, replicates)) |>
    dplyr::ungroup()

  class(out) <- c("cyclingpower_agreement", class(out))
  out
}

#' Both intervals come off one set of resampled indices, so the watts and the
#' per cent on a row describe the same resampled seconds.
#' @noRd
agreement_summary <- function(pair, block, replicates) {
  n <- nrow(pair)
  bias <- mean(pair$difference)
  spread <- stats::sd(pair$difference)

  draws <- draws_over(n, block, replicates)
  bias_ci <- percentile_ci(draws(function(i) mean(pair$difference[i])))
  gain_ci <- percentile_ci(
    draws(function(i) 100 * (exp(mean(pair$log_ratio[i])) - 1))
  )

  tibble::tibble(
    seconds = n,
    bias_w = bias,
    bias_lo = bias_ci[[1]],
    bias_hi = bias_ci[[2]],
    sd_difference = spread,
    loa_lo = bias - 1.96 * spread,
    loa_hi = bias + 1.96 * spread,
    gain_pc = 100 * (exp(mean(pair$log_ratio)) - 1),
    gain_lo = gain_ci[[1]],
    gain_hi = gain_ci[[2]]
  )
}

#' How the difference between two meters varies with power
#'
#' Meters often agree at endurance pace and diverge at threshold or in
#' sprints, which a single average difference hides entirely. Seconds are
#' binned by the mean of the two readings rather than by either one of them:
#' binning on one meter and then plotting its own difference against that bin
#' makes measurement noise look like a real trend.
#'
#' Time is not spread evenly across the power range, so `seconds` is carried
#' through on every row. Bins thinner than `min_seconds` are dropped, and how
#' many seconds that discarded is reported as an attribute.
#'
#' @inheritParams power_agreement
#' @param width Bin width in watts.
#' @param min_seconds Bins with fewer paired seconds than this are dropped.
#' @param replicates Number of bootstrap replicates. Lower than elsewhere in
#'   the package because every band is resampled separately, so 500 across
#'   eight bands is already more work than 1000 over the ride as a whole.
#'
#' @return A tibble with one row per bin per source pair per run:
#'   \describe{
#'     \item{`run`, `source_a`, `source_b`}{As [power_agreement()].}
#'     \item{`band`, `band_range`}{Bin centre in watts, and its label. Seconds
#'       are binned on the mean of the two meters, never on either one.}
#'     \item{`seconds`, `mean_power`}{Paired seconds in the bin, and their mean
#'       level.}
#'     \item{`bias_w`, `bias_lo`, `bias_hi`}{Mean difference in watts within
#'       the bin, with its 95% bootstrap interval.}
#'     \item{`gain_pc`, `gain_lo`, `gain_hi`}{The same as a percentage.}
#'   }
#'   Seconds dropped for thin bins are carried on the `dropped_seconds`
#'   attribute.
#'
#' @family agreement
#' @examples
#' aligned <- read_rides(demo_rides()) |> align_rides()
#' agreement_by_level(aligned)
#' @export
agreement_by_level <- function(
  aligned,
  width = 25,
  min_seconds = 30,
  min_power = 0,
  block = 60,
  replicates = 500
) {
  binned <- paired_seconds(aligned, min_power) |>
    dplyr::mutate(
      band = floor(.data$level / width) * width + width / 2
    )

  out <- binned |>
    dplyr::group_by(.data$run, .data$source_a, .data$source_b, .data$band) |>
    dplyr::group_modify(~ level_summary(.x, block, replicates)) |>
    dplyr::ungroup() |>
    # Labelled here, where width is in scope. Formatting the range downstream
    # means passing width alongside the table and eventually labelling a band
    # with a width it was not binned at.
    dplyr::mutate(
      band_range = sprintf(
        "%g\u2013%g W",
        .data$band - width / 2,
        .data$band + width / 2
      ),
      .after = "band"
    )

  kept <- out$seconds >= min_seconds
  attr(out, "dropped_seconds") <- sum(out$seconds[!kept])
  out[kept, ]
}

#' Power is autocorrelated, so consecutive seconds mostly land in the same
#' band and blocks of rows within a band are largely contiguous in time. The
#' block bootstrap is applied on that basis, both statistics over one set of
#' resampled indices as in `agreement_summary()`.
#' @noRd
level_summary <- function(band, block, replicates) {
  n <- nrow(band)
  draws <- draws_over(n, block, replicates)

  bias_ci <- percentile_ci(draws(function(i) mean(band$difference[i])))
  gain_ci <- percentile_ci(
    draws(function(i) 100 * (exp(mean(band$log_ratio[i])) - 1))
  )

  tibble::tibble(
    seconds = n,
    mean_power = mean(band$level),
    bias_w = mean(band$difference),
    bias_lo = bias_ci[[1]],
    bias_hi = bias_ci[[2]],
    gain_pc = 100 * (exp(mean(band$log_ratio)) - 1),
    gain_lo = gain_ci[[1]],
    gain_hi = gain_ci[[2]]
  )
}

#' Whether a pair's error changes across the power range
#'
#' The finding a single average difference hides, stated as a test rather than
#' left to the eye: the extreme bands' gains, and whether their intervals are
#' separated.
#'
#' The test is on the per-cent gain, not on watts. A meter that is a constant
#' 2.5% out reads further from its partner at 300 W than at 100 W, so a watts
#' comparison of the extreme bands separates for every clean percentage meter.
#' In per cent, that meter is flat and only a level-dependent one separates.
#'
#' Separation is non-overlapping 95% intervals, so there is no threshold
#' constant to argue about; a pair with too few seconds to resolve anything
#' does not fire.
#'
#' @param by_level Result of [agreement_by_level()].
#'
#' @return A tibble with one row per source pair per run, giving the lowest and
#'   highest gain, the bands they came from, and `separated`.
#'
#' @family agreement
#' @examples
#' aligned <- read_rides(demo_rides()) |> align_rides()
#' agreement_by_level(aligned) |> level_dependence()
#' @export
level_dependence <- function(by_level) {
  by_level |>
    dplyr::group_by(.data$run, .data$source_a, .data$source_b) |>
    dplyr::summarise(
      low_band = .data$band_range[which.min(.data$gain_pc)],
      low_gain = min(.data$gain_pc),
      high_band = .data$band_range[which.max(.data$gain_pc)],
      high_gain = max(.data$gain_pc),
      # A band too thin to resample has no interval, so nothing about it is
      # separated from anything. isTRUE() rather than a comparison, so that
      # reads as FALSE rather than travelling downstream as NA.
      separated = isTRUE(
        .data$gain_lo[which.max(.data$gain_pc)] >
          .data$gain_hi[which.min(.data$gain_pc)]
      ),
      .groups = "drop"
    )
}
