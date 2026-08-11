#' Resampled draws of a statistic over an autocorrelated series
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
#' @return Numeric vector of `replicates` draws of the statistic.
#' @noRd
block_bootstrap_draws <- function(n, statistic, block = 60, replicates = 1000) {
  draws_over(n, block, replicates)(statistic)
}

#' The resampled row indices, separated from evaluating a statistic on them so
#' that several statistics over the same seconds can share one set; see
#' `draws_over()`.
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

#' One set of resampled indices, returned as a function that evaluates any
#' statistic over them. Generating the indices dominates the cost, so a second
#' statistic is close to free, and two statistics resampled independently would
#' give intervals that disagree about which seconds they saw.
#'
#' @inheritParams block_bootstrap_draws
#' @return A function of a statistic returning `replicates` draws.
#' @noRd
draws_over <- function(n, block = 60, replicates = 1000) {
  idx <- block_indices(n, block, replicates)

  function(statistic) {
    if (is.null(idx)) {
      rep(NA_real_, replicates)
    } else {
      vapply(idx, statistic, numeric(1))
    }
  }
}

#' Intervals are 95% throughout. Nothing here is a formal test, so a settable
#' confidence level would be a knob without a purpose.
#' @noRd
percentile_ci <- function(draws) {
  stats::quantile(draws, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}
