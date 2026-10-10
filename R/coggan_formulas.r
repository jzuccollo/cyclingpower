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
#' seconds actually spent pedalling, drop the coasting yourself first:
#' `normalised_power(x[x != 0])`.
#'
#' @param power Numeric vector of per-second power in watts.
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
#' average of the seconds actually spent pedalling, drop the coasting yourself
#' first: `average_power(x[x != 0])`.
#'
#' @inheritParams normalised_power
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
#' @inheritParams normalised_power
#' @param ftp Functional threshold power in watts. No default: a number
#'   computed against somebody else's threshold looks exactly like a number
#'   computed against yours.
#'
#' @return Intensity factor, where 1 is riding at threshold.
#'
#' @family metrics
#' @examples
#' intensity_factor(rep(210, 600), ftp = 250)
#' @export
intensity_factor <- function(power, ftp) {
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
tss <- function(power, ftp) {
  t <- length(power)
  intensity_factor <- intensity_factor(power, ftp)
  (t * intensity_factor^2) / 36
}

#' Variability index
#'
#' Normalised power over average power: how ragged the effort was. A steady
#' time trial sits near 1, a criterium or a hilly group ride well above it.
#'
#' @inheritParams normalised_power
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
