#' Confidence interval for a statistic over an autocorrelated series
#'
#' Per-second power is heavily autocorrelated: what you are riding this second
#' is nearly what you were riding last second. Treating an hour's recording as
#' 3,600 independent observations therefore understates the standard error by
#' something like an order of magnitude, which would make a fraction of a watt
#' look decisively different from zero.
#'
#' Resampling contiguous blocks rather than individual seconds keeps the
#' local correlation structure intact, so the interval reflects how much
#' independent information the ride actually carries.
#'
#' @param n Number of paired observations.
#' @param statistic Function of a vector of row indices returning one number.
#' @param block Block length in seconds.
#' @param replicates Number of bootstrap replicates.
#'
#' @return Numeric vector of the lower and upper bounds.
#' @noRd
block_bootstrap_draws <- function(n, statistic, block = 60, replicates = 1000) {
  idx <- block_indices(n, block, replicates)
  if (is.null(idx)) {
    return(rep(NA_real_, replicates))
  }
  vapply(idx, statistic, numeric(1))
}

#' The resampled row indices, separated from evaluating a statistic on them so
#' that two statistics over the same band can share one set of draws. Generating
#' the indices dominates the cost, and two statistics resampled independently
#' would also give intervals that disagree about which seconds they saw.
#'
#' Returns `NULL` when there are too few observations to resample at all.
#'
#' @inheritParams block_bootstrap_draws
#' @return A list of `replicates` integer vectors, each of length `n`.
#' @noRd
block_indices <- function(n, block = 60, replicates = 1000) {
  if (n < 2 * block) {
    block <- max(1, floor(n / 4))
  }
  if (n < 8) {
    return(NULL)
  }

  n_blocks <- ceiling(n / block)
  starts <- seq_len(n - block + 1)

  lapply(seq_len(replicates), function(i) {
    idx <- vapply(
      sample(starts, n_blocks, replace = TRUE),
      function(s) seq(s, length.out = block),
      numeric(block)
    )
    as.integer(idx)[seq_len(n)]
  })
}

#' @rdname block_bootstrap_draws
#' @noRd
block_bootstrap_ci <- function(n, statistic, block = 60, replicates = 1000) {
  percentile_ci(block_bootstrap_draws(n, statistic, block, replicates))
}

#' Intervals are 95% throughout. Nothing here is a formal test, so a settable
#' confidence level would be a knob without a purpose.
#' @noRd
percentile_ci <- function(draws) {
  stats::quantile(draws, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}
