#' The answer, before the table
#'
#' A comparison returns a tibble of estimates and intervals, and the question
#' that sent someone here is a sentence: does this meter read high or low, by
#' how much, and how sure can I be. These methods say that first and then print
#' the tibble as normal, so nothing downstream changes.
#'
#' @param x A result from [power_agreement()] or [chain_sources()].
#' @param ... Passed to the tibble print method.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' aligned <- read_rides(demo_rides()) |> align_rides()
#' power_agreement(aligned)
#' chain_sources(aligned)
#' @name cyclingpower_print
NULL

#' @rdname cyclingpower_print
#' @export
print.cyclingpower_agreement <- function(x, ...) {
  for (i in seq_len(nrow(x))) {
    cat(x$run[[i]], ": ", gain_sentence(x[i, ]), "\n", sep = "")
  }
  cat("\n")
  NextMethod()
}

#' @rdname cyclingpower_print
#' @export
print.cyclingpower_chain <- function(x, ...) {
  for (i in seq_len(nrow(x))) {
    routing <- if (identical(x$basis[[i]], "chained")) {
      paste0(", chained via ", x$via[[i]])
    } else {
      ""
    }
    cat(gain_sentence(x[i, ]), routing, "\n", sep = "")
  }

  # The assumption a chained row rests on cannot be tested by the rides, so it
  # travels with the number rather than waiting in chain_report().
  if (any(x$basis == "chained")) {
    cat(
      "\nA chained pair assumes the bridge read identically on both days.",
      "\nThe rides cannot test that. See chain_report().\n"
    )
  }
  cat("\n")
  NextMethod()
}

#' One row as a sentence.
#'
#' Meters are compared as a gain, so the direction is about the ratio rather
#' than the watts: a meter 2.6% low is 2.6% low at every level, whereas the
#' watts it is short by depend on how hard you were going.
#'
#' The interval is turned to face the same way as the point estimate, so
#' "higher" is described by a higher-is-positive interval. An interval that
#' still straddles zero after that says the direction is not resolved.
#' @noRd
gain_sentence <- function(row) {
  higher <- row$gain_pc >= 0
  gain <- abs(row$gain_pc)
  lo <- if (higher) row$gain_lo else -row$gain_hi
  hi <- if (higher) row$gain_hi else -row$gain_lo

  seconds <- if ("seconds" %in% names(row)) {
    sprintf(", from %s seconds", format(row$seconds, big.mark = ","))
  } else {
    ""
  }

  sprintf(
    "%s reads %.1f%% %s than %s (95%% CI %.1f to %.1f%s)",
    row$source_a,
    gain,
    if (higher) "higher" else "lower",
    row$source_b,
    lo,
    hi,
    seconds
  )
}
