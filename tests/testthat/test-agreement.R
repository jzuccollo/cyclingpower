two_source_rides <- function(...) {
  as_rides(list(A = fixture_ride(), B = make_source(fixture_ride(), ...)))
}

test_that("seconds are levelled on the pair mean, not on one meter", {
  # The Bland-Altman choice. Binning on one meter and plotting its own
  # difference against those bins turns that meter's noise into a false slope,
  # so this is tested where the decision is made rather than at the chart.
  aligned <- align_rides(two_source_rides(gain = 1.03))
  pairs <- paired_seconds(aligned)

  expect_equal(pairs$level, (pairs$a + pairs$b) / 2)
  expect_equal(pairs$difference, pairs$a - pairs$b)
})

test_that("a known gain is recovered", {
  # B reads 3% high, so A relative to B reads 1/1.03 - 1 = -2.91%.
  aligned <- align_rides(two_source_rides(gain = 1.03))
  out <- withr::with_seed(1, power_agreement(aligned))

  expect_equal(nrow(out), 1)
  expect_equal(out$source_a, "A")
  expect_equal(out$source_b, "B")
  expect_equal(out$gain_pc, 100 * (1 / 1.03 - 1), tolerance = 1e-6)

  # A pure gain gives every second the same log ratio, so there is no sampling
  # variability left for the bootstrap to find and the interval collapses.
  expect_equal(out$gain_lo, out$gain_pc, tolerance = 1e-6)
  expect_equal(out$gain_hi, out$gain_pc, tolerance = 1e-6)
})

test_that("a noisy gain gives an interval that brackets the truth", {
  aligned <- align_rides(two_source_rides(gain = 1.03, noise = 12))
  out <- withr::with_seed(1, power_agreement(aligned))

  truth <- 100 * (1 / 1.03 - 1)
  expect_equal(out$gain_pc, truth, tolerance = 0.5)
  expect_lt(out$gain_lo, truth)
  expect_gt(out$gain_hi, truth)
})

test_that("a known watt offset is recovered", {
  aligned <- align_rides(two_source_rides(offset_w = 12))
  out <- withr::with_seed(1, power_agreement(aligned))

  expect_equal(out$bias_w, -12, tolerance = 1e-6)
  expect_equal(out$sd_difference, 0, tolerance = 1e-6)
})

test_that("gain and offset survive a clock offset", {
  aligned <- align_rides(two_source_rides(gain = 1.03, lag = 9))
  out <- withr::with_seed(1, power_agreement(aligned))

  expect_equal(out$gain_pc, 100 * (1 / 1.03 - 1), tolerance = 1e-6)

  # A gain leaves a difference proportional to power, so the spread of the
  # difference is the gain times the spread of power, not zero.
  paired <- fixture_ride()$power
  paired <- paired[paired > 0]
  expect_equal(out$sd_difference, 0.03 * stats::sd(paired), tolerance = 1e-6)
})

test_that("identical meters show no difference and zero spread", {
  aligned <- align_rides(two_source_rides())
  out <- withr::with_seed(1, power_agreement(aligned))

  expect_equal(out$bias_w, 0)
  expect_equal(out$gain_pc, 0)
  expect_equal(out$loa_lower, 0)
  expect_equal(out$loa_upper, 0)
})

test_that("coasting seconds are excluded from the pairing", {
  aligned <- align_rides(two_source_rides(gain = 1.03))
  out <- withr::with_seed(1, power_agreement(aligned))

  ride <- fixture_ride()
  expect_equal(out$seconds, sum(ride$power > 0))
  expect_lt(out$seconds, nrow(ride))
})

test_that("the block bootstrap is wider than treating seconds as independent", {
  # A gain error leaves a difference proportional to power, and power is
  # strongly autocorrelated, so this is the case the block bootstrap exists
  # for. Independent noise would not be, since the bootstrap is right to treat
  # that as carrying full information per second.
  aligned <- align_rides(two_source_rides(gain = 1.03))
  out <- withr::with_seed(1, power_agreement(aligned))

  naive_half_width <- 1.96 * out$sd_difference / sqrt(out$seconds)
  bootstrap_half_width <- (out$bias_hi - out$bias_lo) / 2

  # If these agree, the bootstrap is not capturing the autocorrelation and the
  # interval understates the uncertainty.
  expect_gt(bootstrap_half_width, 3 * naive_half_width)
})

test_that("a power-dependent bias is located, where an average hides it", {
  a <- fixture_ride()
  b <- make_level_dependent(fixture_ride(), 1.02, 0.99, threshold = 150)
  aligned <- align_rides(as_rides(list(A = a, B = b)))

  by_level <- withr::with_seed(1, agreement_by_level(aligned, width = 25))

  low <- by_level[by_level$mean_power < 130, ]
  high <- by_level[by_level$mean_power > 200, ]

  expect_gt(nrow(low), 0)
  expect_gt(nrow(high), 0)
  # B reads high at low power, so A minus B is negative there, and positive up top.
  expect_true(all(low$bias_w < 0))
  expect_true(all(high$bias_w > 0))
})

test_that("thin bins are dropped and the loss is recorded", {
  aligned <- align_rides(two_source_rides(gain = 1.03))
  by_level <- withr::with_seed(
    1,
    agreement_by_level(aligned, min_seconds = 200)
  )

  expect_true(all(by_level$seconds >= 200))
  expect_gt(attr(by_level, "dropped_seconds"), 0)
})

test_that("three sources give one row per pair", {
  rides <- as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), gain = 1.02),
    C = make_source(fixture_ride(), gain = 0.98)
  ))
  out <- withr::with_seed(1, power_agreement(align_rides(rides)))

  expect_equal(nrow(out), 3)
  expect_equal(paste(out$source_a, out$source_b), c("A B", "A C", "B C"))
})

test_that("a pure gain is flat in per cent and rising in watts", {
  # The claim the per-cent scale exists to make. B reads 3% high, so A minus B
  # grows steadily more negative as the power rises, purely because 3% of
  # 300 W exceeds 3% of 100 W. Reading that as a meter whose error depends on
  # effort is the mistake the two scales exist to separate: the same meter is
  # a straight slope in watts and a flat line in per cent.
  aligned <- align_rides(two_source_rides(gain = 1.03, noise = 2))
  by_level <- withr::with_seed(1, agreement_by_level(aligned, width = 25))

  ordered <- by_level[order(by_level$band), ]
  expect_lt(stats::cor(ordered$band, ordered$bias_w), -0.9)
  expect_gt(max(abs(by_level$bias_w)) / min(abs(by_level$bias_w)), 3)
  expect_lt(max(by_level$gain_pc) - min(by_level$gain_pc), 0.5)

  expect_false(level_dependence(by_level)$separated)
})

test_that("the caveat fires for the planted quarq and not for the kickr", {
  # The distinction the demo plants, and the one the app shows: kickr is a
  # clean 2.5% gain and quarq changes sign at 200 W. Both meters carry noise
  # here, which is the realistic case — the synthetic helper puts all of it on
  # one source, and at larger amplitudes that asymmetry alone bends the log
  # ratio enough to separate the extreme bands.
  aligned <- read_rides(
    ride_spec_from_names(list.files(demo_rides(), full.names = TRUE))
  ) |>
    align_rides()
  uneven <- withr::with_seed(1, agreement_by_level(aligned)) |>
    level_dependence()

  expect_false(uneven$separated[uneven$source_b == "kickr"])
  expect_true(uneven$separated[uneven$source_b == "quarq"])
})

test_that("a level-dependent meter separates in per cent", {
  a <- fixture_ride()
  b <- make_level_dependent(fixture_ride(), 1.02, 0.99, threshold = 150)
  aligned <- align_rides(as_rides(list(A = a, B = b)))
  by_level <- withr::with_seed(1, agreement_by_level(aligned, width = 25))

  expect_true(level_dependence(by_level)$separated)
})

test_that("band labels carry the width they were binned at", {
  aligned <- align_rides(two_source_rides(gain = 1.03))
  by_level <- withr::with_seed(1, agreement_by_level(aligned, width = 25))

  expect_equal(by_level$band_range[by_level$band == 87.5], "75–100 W")

  wider <- withr::with_seed(1, agreement_by_level(aligned, width = 50))
  expect_equal(wider$band_range[wider$band == 75], "50–100 W")
})

test_that("the same seed gives the same band intervals", {
  # Guards the block_indices() extraction: the resampled draws are now shared
  # between the two statistics, so a change in how they are generated would
  # move every published interval at once.
  aligned <- align_rides(two_source_rides(gain = 1.03, noise = 6))
  once <- withr::with_seed(2, agreement_by_level(aligned, width = 25))
  twice <- withr::with_seed(2, agreement_by_level(aligned, width = 25))

  expect_equal(once$gain_lo, twice$gain_lo)
  expect_equal(once$gain_hi, twice$gain_hi)
  expect_true(all(once$gain_lo <= once$gain_pc))
  expect_true(all(once$gain_hi >= once$gain_pc))
})
