#' Simulate a power series with a given average and normalised power
#'
#' Rather than summarising a ride you rode, it builds a
#' second-by-second series that would produce the specified numbers.
#'
#' The problem is underdetermined so the shape comes from an
#' autocorrelated process, which is what riding looks like: you hold a power for
#' minutes at a time rather than jumping about at random.
#'
#' Scale and shape are then solved separately. Both metrics scale linearly with power, so their ratio does not
#' depend on the overall level. Raising the series to a power sets the
#' variability index, and multiplying by a constant sets the average.
#'
#' @param n_seconds Number of seconds to simulate.
#' @param target_ap Average power to hit, in watts.
#' @param target_np Normalised power to hit, in watts. Cannot be below
#'   `target_ap`.
#' @param smoothness Autocorrelation of the underlying effort, between 0 and 1.
#'   Higher values give long sustained efforts, lower values a more punchy ride.
#'   At a given normalised power this changes the character of the session, not
#'   its statistics.
#'
#' @return Numeric vector of per-second power values, hitting both targets
#'   exactly. Values are not rounded to whole watts, which would perturb them.
#'
#' @section Feasibility:
#' Nothing here knows about critical power, so nothing stops you naming targets
#' no rider could hold. A normalised power above threshold sustained for an hour
#' comes back as a series that would exhaust any W' several times over, and that
#' is the target being impossible rather than the simulation being wrong: a
#' *steady* hour at that power would fail the same test. Check the intensity you
#' are asking for before reading anything into the shape that comes back.
#'
#' Within a feasible intensity the shape holds up, but its upper tail is
#' lognormal and widens with the variability index, so a ragged ride posts
#' sustained peaks — the best thirty seconds, not the peak second — beyond what
#' the same ride's five-minute best implies. At a high variability index the
#' series stops being a plausible ride and is only a series with the right two
#' numbers.
#'
#' @examples
#' ride <- withr::with_seed(1, simulate_power(3600, target_ap = 180, target_np = 220))
#' c(average_power(ride), normalised_power(ride))
#' @export
simulate_power <- function(
  n_seconds,
  target_ap,
  target_np,
  smoothness = 0.98
) {
  target_vi <- target_np / target_ap

  if (target_vi < 1) {
    stop(
      "Normalised power cannot be below average power: no series has a ",
      "variability index under 1."
    )
  }

  shape <- effort_shape(n_seconds, smoothness)
  shaped <- shape^solve_spikiness(shape, target_vi, smoothness)

  shaped * target_ap / average_power(shaped)
}

#' A positive, autocorrelated series standing in for the shape of an effort.
#' Standardising before exponentiating fixes the spread, so `smoothness`
#' controls only how sustained the changes are and the exponent alone sets the
#' variability index.
#' @noRd
effort_shape <- function(n_seconds, smoothness) {
  z <- as.numeric(stats::arima.sim(list(ar = smoothness), n = n_seconds))
  exp(as.numeric(scale(z)))
}

#' Raising a positive series to a higher power stretches its peaks relative to
#' its troughs, so the variability index climbs monotonically from exactly 1 at
#' an exponent of zero. That gives a guaranteed bracket for the root find.
#' @noRd
solve_spikiness <- function(shape, target_vi, smoothness) {
  if (target_vi <= 1 + 1e-9) {
    return(0)
  }

  gap <- function(b) variability_index(shape^b) - target_vi

  upper <- 1
  while (gap(upper) < 0) {
    upper <- upper * 2
    if (upper > 64) {
      stop(
        "Could not reach a variability index of ",
        signif(target_vi, 4),
        " with smoothness = ",
        smoothness,
        ". Raise smoothness so the effort holds longer, since noise inside ",
        "the 30-second window cannot lift normalised power."
      )
    }
  }

  stats::uniroot(gap, c(0, upper), tol = 1e-10)$root
}
