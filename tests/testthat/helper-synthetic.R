# A single real FIT file tells us the code runs, not that it is right. These
# helpers build sources with distortions we choose, so a test can assert that
# the machinery recovers a number we already know.

# FITfileR is not on CRAN, so a clean machine has the rest of the suite but not
# this. Skipping beats erroring: the tests that need a real ride say so, and
# everything built on demo_rides() still runs.
fixture_ride <- function() {
  testthat::skip_if_not_installed("FITfileR")
  read_file(system.file("extdata", "power1.fit", package = "cyclingpower"))
}

#' @param gain Multiplicative scaling, the usual shape of a meter's error.
#' @param offset_w Additive watts.
#' @param lag Seconds the source's clock runs ahead of true time.
#' @param noise SD of independent measurement noise.
make_source <- function(
  ride,
  gain = 1,
  offset_w = 0,
  lag = 0,
  noise = 0,
  seed = 1
) {
  withr::with_seed(seed, {
    ride$power <- ride$power * gain + offset_w
    if (noise > 0) {
      ride$power <- pmax(0, ride$power + stats::rnorm(nrow(ride), 0, noise))
    }
  })
  ride$time <- ride$time + lag
  ride
}

as_rides <- function(sources, run = "run1") {
  Map(
    function(x, nm) dplyr::mutate(x, run = run, source = nm, .before = 1),
    sources,
    names(sources)
  ) |>
    dplyr::bind_rows()
}

#' A meter whose error changes across the power range, which is the pattern a
#' single average difference hides.
make_level_dependent <- function(ride, low_gain, high_gain, threshold) {
  ride$power <- ride$power *
    ifelse(ride$power < threshold, low_gain, high_gain)
  ride
}

#' A steady effort with no sharp features, recorded twice with independent
#' noise: the case where a clock offset cannot be identified.
featureless_rides <- function(n = 600) {
  base <- fixture_ride()[seq_len(n), ]
  base$power <- 200 + 5 * sin(seq_len(n) / 300)

  as_rides(list(
    A = make_source(base, noise = 8, seed = 1),
    B = make_source(base, noise = 8, seed = 2)
  ))
}
