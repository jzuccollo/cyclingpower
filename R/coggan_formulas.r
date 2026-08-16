#' Drop coasting seconds
#'
#' A utility for the times you want a metric over pedalling only. The metrics
#' in this package follow the standard definitions and count coasting, so
#' compose this yourself where you want the other behaviour:
#' `average_power(drop_zeros(x))`.
#'
#' @param power Numeric vector of per-second power in watts.
#'
#' @return The input with zeros removed.
#'
#' @family metrics
#' @examples
#' drop_zeros(c(200, 0, 220, 0, 210))
#' @export
drop_zeros <- function(power) {
  power[power != 0]
}

#' Normalised power
#'
#' The fourth-power mean of a 30-second rolling average, which weights hard
#' efforts more heavily than a plain average does and so tracks the
#' physiological cost of a ragged ride better than average power.
#'
#' Follows Coggan's definition: the rolling mean runs over the whole recording
#' with coasting included and counted as zero, and trails rather than centres,
#' so the first value falls at the thirty-second mark. A recording shorter than
#' the window has no normalised power and returns `NA`. For a value over the
#' seconds actually spent pedalling, compose with [drop_zeros()].
#'
#' @inheritParams drop_zeros
#'
#' @return Normalised power in watts.
#'
#' @family metrics
#' @examples
#' normalised_power(rep(250, 600))
#' @export
normalised_power <- function(power) {
  if (length(power) < 30) {
    return(NA_real_)
  }
  rolling <- zoo::rollmean(power, 30, align = "right")
  mean(rolling^4, na.rm = TRUE)^0.25
}

#' Average power
#'
#' The mean over the whole recording, counting coasting as zero, which is what
#' variability index and training stress score are defined against. For the
#' average of the seconds actually spent pedalling, compose with
#' [drop_zeros()].
#'
#' @inheritParams drop_zeros
#'
#' @return Average power in watts.
#'
#' @family metrics
#' @examples
#' average_power(c(rep(200, 1800), rep(0, 1800)))
#' @export
average_power <- function(power) {
  mean(power, na.rm = TRUE)
}

#' Intensity factor
#'
#' Normalised power as a fraction of threshold power.
#'
#' @inheritParams drop_zeros
#' @param ftp Functional threshold power in watts. The default is the
#'   package's reference rider and is almost certainly not you: set your own
#'   for a number that means anything.
#'
#' @return Intensity factor, where 1 is riding at threshold.
#'
#' @family metrics
#' @examples
#' intensity_factor(rep(210, 600), ftp = 250)
#' @export
intensity_factor <- function(power, ftp = 250) {
  normalised_power(power) / ftp
}

#' Training stress score
#'
#' One hour at threshold scores 100. Duration is taken as the number of
#' observations, so the data must be sampled once per second; smart recording
#' will understate it.
#'
#' @inheritParams intensity_factor
#'
#' @return Training stress score.
#'
#' @family metrics
#' @examples
#' tss(rep(250, 3600), ftp = 250)
#' @export
tss <- function(power, ftp = 250) {
  t <- length(power)
  intensity_factor <- intensity_factor(power, ftp)
  (t * intensity_factor^2) / 36
}

#' Variability index
#'
#' Normalised power over average power: how ragged the effort was. A steady
#' time trial sits near 1, a criterium or a hilly group ride well above it.
#'
#' @inheritParams drop_zeros
#'
#' @return Variability index, where 1 is a perfectly steady effort.
#'
#' @family metrics
#' @examples
#' variability_index(rep(250, 600))
#' @export
variability_index <- function(power) {
  normalised_power(power) / average_power(power)
}
